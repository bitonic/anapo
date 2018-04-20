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
import Data.IORef (IORef, readIORef, atomicModifyIORef', newIORef, writeIORef)
import qualified Control.Concurrent.Async as Async
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Monad.State (runStateT, put, get)
import qualified Data.HashMap.Strict as HMS
import GHC.Stack (CallStack, SrcLoc(..), getCallStack)
import Control.Exception.Safe (try)

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

data DispatchMsg stateRoot = forall props context state. DispatchMsg
  { _dispatchMsgTraverseComp :: AffineTraversal' (Component () () stateRoot) (Component props context state)
  , _dispatchMsgModify :: context -> state -> DOM.JSM (state, V.Rerender)
  , _dispatchCallStack :: CallStack
  }

nodeLoop :: forall st.
     (forall a. (st -> DOM.JSM a) -> Action () st a)
  -> Node () st
  -- ^ how to render the state
  -> (SomeException -> Node () ())
  -- ^ how to render exceptions
  -> DOM.Node
  -- ^ where to place the node
  -> DOM.JSM (V.Node RenderedVDomNode)
  -- ^ returns the final rendered node, when there is nothing left to
  -- do. might never terminate
nodeLoop withState node excComp root = do
  -- dispatch channel
  dispatchChan :: Chan (DispatchMsg st) <- liftIO newChan
  let
    getMsg = do
      mbF :: Either BlockedIndefinitelyOnMVar (DispatchMsg st) <- tryAsync (readChan dispatchChan)
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
    actionEnv :: ActionEnv (Component () () st)
    actionEnv = ActionEnv
      { aeRegisterThread = register
      , aeHandleException = handler
      , aeDispatch = Dispatch (\stack travComp modify -> writeChan dispatchChan (DispatchMsg travComp modify stack))
      }
  let
    actionTrav ::
         AffineTraversal' (Component () () st) (Component props ctx' st')
      -> ActionTraverse (Component () () st) props ctx' st' ctx' st'
    actionTrav travComp = ActionTraverse
      { atToComp = travComp
      , atToState = id
      , atToContext = id
      }
  -- helper to run the component
  let
    runComp ::
         Maybe (Component () () st)
      -> VDomPath
      -> AffineTraversal' (Component () () st) (Component props ctx' st')
      -> Component props ctx' st'
      -> props
      -> DOM.JSM (V.Node V.SomeVDomNode)
    runComp mbPrevComp path travComp comp props = do
      Just ctx <- liftIO (readIORef (_componentContext comp))
      ((_, vdom), vdomDt) <- timeIt $ unDomM
        (do
          node0 <- _componentNode comp props
          V.forSomeNodeBody node0 $ \node' -> do
            patches <- registerComponent (_componentPositions comp) props
            patchNode patches node')
        actionEnv
        (actionTrav travComp)
        DomEnv
          { domEnvReversePath = reverse path
          , domEnvPrevState = mbPrevComp
          , domEnvDirtyPath = False
          }
        ctx
        (_componentState comp)
        ()
      DOM.syncPoint
      logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
      return vdom
  -- compute state
  unAction
    (withState $ \st0 -> do
      -- what to do in case of exceptions
      let
        onErr :: V.Node RenderedVDomNode -> SomeException -> DOM.JSM a
        onErr rendered err = do
          -- if we got an exception, render one last time and shut down
          logError ("Got exception, will render it and rethrow: " <> pack (show err))
          vdom <- simpleNode () (excComp err)
          void (reconciliateVirtualDom rendered [] vdom)
          logError ("Just got exception and rendered, will rethrow: " <> pack (show err))
          liftIO (throwIO err)
      -- main loop
      let
        go ::
             Component () () st
          -- ^ the previous state
          -> V.Node RenderedVDomNode
          -- ^ the previous rendered node
          -> DOM.JSM (V.Node RenderedVDomNode)
        go compRoot !rendered = do
          -- get the next update or the next exception. we are biased
          -- towards exceptions since we want to exit immediately when
          -- there is a failure.
          fOrErr :: Either SomeException (Maybe (DispatchMsg st)) <- liftIO (Async.race (readMVar excVar) getMsg)
          case fOrErr of
            Left err -> onErr rendered err
            Right Nothing -> do
              logInfo "No state update received, terminating component loop"
              return rendered
            Right (Just (DispatchMsg travComp modif stack)) -> do
              logDebug (addCallStack stack "About to update state")
              -- traverse to the component using StateT, failing
              -- if we reach anything twice (it'd mean it's not an
              -- AffineTraversal)
              ((compRoot', mbComp), updateDt) <- timeIt $ runStateT
                (travComp
                  (\comp -> do
                      mbComp <- get
                      case mbComp of
                        Just{} -> do
                          -- fail if we already visited
                          fail "nodeLoop: visited multiple elements in the affine traversal for component! check if your AffineTraversal are really affine"
                        Nothing -> do
                          Just ctx <- liftIO (readIORef (_componentContext comp))
                          mbSt <- DOM.liftJSM (try (modif ctx (_componentState comp)))
                          case mbSt of
                            Left err -> DOM.liftJSM (onErr rendered err)
                            Right (st, rerender) -> do
                              let comp' = comp{ _componentState = st }
                              put (Just (comp', rerender))
                              return comp')
                  compRoot)
                Nothing
              logDebug ("State updated (" <> pack (show updateDt) <> "), might re render")
              case mbComp of
                Nothing -> do
                  logInfo "The component was not found, not rerendering"
                  go compRoot' rendered
                Just (comp, rerender) -> do
                  rendered' <- case rerender of
                    V.UnsafeDontRerender -> return rendered
                    V.Rerender -> do
                      positions <- liftIO (readIORef (_componentPositions comp))
                      logDebug ("Rendering component " <> _componentName comp <> " at " <> pack (show (HMS.size positions)) <> " positions")
                      foldM
                        (\rendered' (pos, props) -> do
                            vdom <- runComp (Just compRoot) pos travComp comp props
                            reconciliateVirtualDom rendered' pos vdom)
                        rendered
                        (HMS.toList positions)
                  go compRoot' rendered'
      tid <- liftIO myThreadId
      finally
        (do
          -- run for the first time
          comp <- newNamedComponent "root" st0 (\() -> node)
          liftIO (writeIORef (_componentContext comp) (Just ()))
          vdom <- runComp Nothing [] id comp ()
          rendered0 <- renderVirtualDom vdom $ \rendered -> do
            DOM.Node.appendChild_ root (renderedVDomNodeDom (V.nodeBody rendered))
            return rendered
          -- now loop
          go comp rendered0)
        (liftIO (readIORef tidsRef >>= traverse_ (\tid' -> when (tid /= tid') (killThread tid')))))
    actionEnv
    (actionTrav id)

installNodeBody ::
     (forall a. (st -> DOM.JSM a) -> Action () st a)
  -> Node () st
  -> (SomeException -> Node () ())
  -> DOM.JSM ()
installNodeBody getSt vdom0 excVdom = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.Document.getBodyUnchecked doc
  void (nodeLoop getSt vdom0 excVdom (DOM.toNode body))

addCallStack :: CallStack -> Text -> Text
addCallStack stack = case getCallStack stack of
  [] -> id
  (_, SrcLoc{..}) : _ -> \txt -> "[" <> pack srcLocModule <> ":" <> pack (show srcLocStartLine) <> ":" <> pack (show srcLocStartCol) <> "] " <> txt
