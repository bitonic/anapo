{-# LANGUAGE OverloadedStrings #-}
module Anapo.Loop
  ( nodeLoop
  , installNodeBody
  ) where

import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when, foldM)
import Data.Foldable (traverse_)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Monoid ((<>))
import Control.Exception.Safe (throwIO, tryAsync, SomeException, bracket, finally)
import Control.Exception (BlockedIndefinitelyOnMVar)
import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, readMVar)
import Data.IORef (IORef, readIORef, atomicModifyIORef', newIORef)
import qualified Control.Concurrent.Async as Async
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Monad.State (runStateT, put, get)
import qualified Data.HashMap.Strict as HMS
import GHC.Stack (CallStack, SrcLoc(..), getCallStack)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM.Node
import qualified GHCJS.DOM.Document as DOM.Document

import qualified Anapo.VDOM as V
import Anapo.Component.Internal
import Anapo.Render
import Anapo.Text (Text, pack)
import Anapo.Logging

timeIt :: DOM.JSM a -> DOM.JSM (a, NominalDiffTime)
timeIt m = do
  t0 <- liftIO getCurrentTime
  x <- m
  t1 <- liftIO getCurrentTime
  return (x, diffUTCTime t1 t0)

data DispatchMsg stateRoot = forall props state. DispatchMsg
  { _dispatchMsgTraverse :: AffineTraversal' (Component () stateRoot) (Component props state)
  , _dispatchMsgModify :: state -> DOM.JSM state
  , _dispatchCallStack :: CallStack
  }

nodeLoop :: forall state.
     (forall a. (state -> DOM.JSM a) -> Action state a)
  -> Node state
  -- ^ how to render the state
  -> (SomeException -> Node ())
  -- ^ how to render exceptions
  -> DOM.Node
  -- ^ where to place the node
  -> DOM.JSM (V.Node RenderedVDomNode)
  -- ^ returns the final rendered node, when there is nothing left to
  -- do. might never terminate
nodeLoop withState node excComp root = do
  -- dispatch channel
  dispatchChan :: Chan (DispatchMsg state) <- liftIO newChan
  let
    getMsg = do
      mbF :: Either BlockedIndefinitelyOnMVar (DispatchMsg state) <- tryAsync (readChan dispatchChan)
      case mbF of
        Left{} -> do
          logInfo "got undefinitedly blocked on mvar on dispatch channel, will stop"
          return Nothing
        Right f -> return (Just f)
  -- exception mvar
  excVar :: MVar SomeException <- liftIO newEmptyMVar
  let handler err = void (tryPutMVar excVar err)
  -- set of threads
  tidsRef :: IORef (HashSet ThreadId) <- liftIO (newIORef mempty)
  let
    register m = do
      tid <- myThreadId
      bracket
        (atomicModifyIORef' tidsRef (\tids -> (HS.insert tid tids, ())))
        (\_ -> atomicModifyIORef' tidsRef (\tids -> (HS.delete tid tids, ())))
        (\_ -> m)
  let
    actionEnv ::
         AffineTraversal' (Component () state) (Component props state')
      -> ActionEnv (Component () state) props state' state'
    actionEnv trav = ActionEnv
      { aeRegisterThread = register
      , aeHandleException = handler
      , aeDispatch = Dispatch (\stack trav' modify -> writeChan dispatchChan (DispatchMsg trav' modify stack))
      , aeTraverseToComp = trav
      , aeTraverseToState = id
      }
  -- helper to run the component
  let
    runComp ::
        Maybe (Component () state)
     -> VDomPath
     -> AffineTraversal' (Component () state) (Component props state')
     -> Component props state'
     -> props
     -> DOM.JSM (V.Node V.SomeVDomNode)
    runComp mbPrevComp path trav comp props = do
      ((_, vdom), vdomDt) <- timeIt $ unAnapoM
        (do
          node0 <- _componentNode comp props
          forSomeNodeBody node0 $ \node' -> do
            patches <- registerComponent (_componentPositions comp) props
            patchNode patches node')
        (actionEnv trav)
        AnapoEnv
          { aeReversePath = reverse path
          , aePrevState = mbPrevComp
          , aeState = _componentState comp
          }
        ()
      DOM.syncPoint
      logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
      return vdom
  -- compute state
  unAction
    (withState $ \st0 -> do
      -- main loop
      let
        go ::
             Component () state
          -- ^ the previous state
          -> V.Node RenderedVDomNode
          -- ^ the previous rendered node
          -> DOM.JSM (V.Node RenderedVDomNode)
        go compRoot !rendered = do
          -- get the next update or the next exception. we are biased
          -- towards exceptions since we want to exit immediately when
          -- there is a failure.
          fOrErr :: Either SomeException (Maybe (DispatchMsg state)) <- liftIO (Async.race (readMVar excVar) getMsg)
          case fOrErr of
            Left err -> do
              -- if we got an exception, render one last time and shut down
              logError ("Got exception, will render it and rethrow: " <> pack (show err))
              vdom <- simpleNode () (excComp err)
              void (reconciliateVirtualDom rendered [] vdom)
              logError ("Just got exception and rendered, will rethrow: " <> pack (show err))
              liftIO (throwIO err)
            Right Nothing -> do
              logInfo "No state update received, terminating component loop"
              return rendered
            Right (Just (DispatchMsg trav modif stack)) -> do
              logDebug (addCallStack stack "About to update state")
              -- traverse to the component using StateT, failing
              -- if we reach anything twice (it'd mean it's not an
              -- AffineTraversal)
              ((compRoot', mbComp), updateDt) <- timeIt $ runStateT
                (trav
                  (\comp -> do
                      mbComp <- get
                      case mbComp of
                        Just{} -> do
                          -- fail if we already visited
                          fail "nodeLoop: visited multiple elements in the affine traversal for component! check if your AffineTraversal are really affine"
                        Nothing -> do
                          st <- DOM.liftJSM (modif (_componentState comp))
                          let comp' = comp{ _componentState = st }
                          put (Just comp')
                          return comp')
                  compRoot)
                Nothing
              logDebug ("State updated (" <> pack (show updateDt) <> "), might re render")
              case mbComp of
                Nothing -> do
                  logInfo "The component was not found, not rerendering"
                  go compRoot' rendered
                Just comp -> do
                  positions <- liftIO (readIORef (_componentPositions comp))
                  logDebug ("Rendering at " <> pack (show (HMS.size positions)) <> " positions")
                  rendered' <- foldM
                    (\rendered' (pos, props) -> do
                        vdom <- runComp (Just compRoot) pos trav comp props
                        reconciliateVirtualDom rendered' pos vdom)
                    rendered
                    (HMS.toList positions)
                  go compRoot' rendered'
      tid <- liftIO myThreadId
      finally
        (do
          -- run for the first time
          comp <- newComponent st0 (\() -> node)
          vdom <- runComp Nothing [] id comp ()
          rendered0 <- renderVirtualDom vdom $ \rendered -> do
            DOM.Node.appendChild_ root (renderedVDomNodeDom (V.nodeBody rendered))
            return rendered
          -- now loop
          go comp rendered0)
        (liftIO (readIORef tidsRef >>= traverse_ (\tid' -> when (tid /= tid') (killThread tid')))))
    (actionEnv id)

installNodeBody ::
     (forall a. (state -> DOM.JSM a) -> Action state a)
  -> Node state
  -> (SomeException -> Node ())
  -> DOM.JSM ()
installNodeBody getSt vdom0 excVdom = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.Document.getBodyUnchecked doc
  void (nodeLoop getSt vdom0 excVdom (DOM.toNode body))

addCallStack :: CallStack -> Text -> Text
addCallStack stack = case getCallStack stack of
  [] -> id
  (_, SrcLoc{..}) : _ -> \txt -> "[" <> pack srcLocModule <> ":" <> pack (show srcLocStartLine) <> ":" <> pack (show srcLocStartCol) <> "] " <> txt
