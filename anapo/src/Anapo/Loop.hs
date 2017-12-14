{-# LANGUAGE OverloadedStrings #-}
module Anapo.Loop
  ( nodeLoop
  , installNodeBody
  ) where

import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
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
import qualified Data.Foldable as F
import Control.Monad.State (runStateT)
import Control.Lens (use, _1, _2, (.=))
import Data.Foldable (sequenceA_)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM.Node
import qualified GHCJS.DOM.Document as DOM.Document

import qualified Anapo.VDOM as V
import Anapo.Component.Internal
import Anapo.Render
import Anapo.Text (pack)
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
    get = do
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
      , aeDispatch = Dispatch (\trav' modify -> writeChan dispatchChan (DispatchMsg trav' modify))
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
     -> DOM.JSM (ClearPlacedComponents, V.Node V.SomeVDomNode)
    runComp mbPrevComp path trav comp props = do
      ((comps, _, vdom), vdomDt) <- timeIt $ unAnapoM
        (do
          registerComponent comp props
          _componentNode comp props)
        (actionEnv trav)
        AnapoEnv
          { aeReversePath = reverse path
          , aePrevState = mbPrevComp
          , aeState = _componentState comp
          }
        [] ()
      DOM.syncPoint
      logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
      return (comps, vdom)
  -- compute state
  unAction
    (withState $ \st0 -> do
      -- main loop
      let
        go ::
             Component () state
          -- ^ the previous state
          -> ClearPlacedComponents
          -- ^ the previous placed components
          -> V.Node RenderedVDomNode
          -- ^ the previous rendered node
          -> DOM.JSM (V.Node RenderedVDomNode)
        go compRoot !compsPoss !rendered = do
          -- get the next update or the next exception. we are biased
          -- towards exceptions since we want to exit immediately when
          -- there is a failure.
          fOrErr :: Either SomeException (Maybe (DispatchMsg state)) <- liftIO (Async.race (readMVar excVar) get)
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
            Right (Just (DispatchMsg trav modif)) -> do
              -- traverse to the component using StateT, failing
              -- if we reach anything twice (it'd mean it's not an
              -- AffineTraversal)
              ((compRoot', (_, mbToRender)), updateDt) <- timeIt $ runStateT
                (trav
                  (\comp -> do
                      -- fail if we already visited
                      alreadyVisited <- use _1
                      when alreadyVisited $
                        fail "nodeLoop: visited multiple elements in the affine traversal for component! check if your AffineTraversal are really affine"
                      -- remember that we have already visited
                      _1 .= True
                      -- perform the update
                      st <- DOM.liftJSM (modif (_componentState comp))
                      let comp' = comp{ _componentState = st }
                      -- check if the component is present in the
                      -- vdom at all. if it's not, we won't need to
                      -- render.
                      mbPos <- liftIO (readIORef (_componentPlaced comp))
                      F.for_ mbPos $ \(props, pos) ->
                        -- signal which component we need to render,
                        -- and at which position. note that we
                        -- need to grab the position now before
                        -- rendering, since it might be reset while
                        -- rendering.
                        _2 .= Just (comp', props, pos)
                      return comp')
                  compRoot)
                (False, Nothing)
              logDebug ("State updated (" <> pack (show updateDt) <> "), might re render")
              -- reset all the components
              liftIO (sequenceA_ compsPoss)
              case mbToRender of
                Nothing -> do
                  logInfo "Did not get anything to render, either because the component was not found or because it was not placed."
                  go compRoot' [] rendered
                Just (comp, props, pos) -> do
                  logInfo ("POSITION: " <> pack (show pos))
                  (compsPoss', vdom) <- runComp (Just compRoot) pos trav comp props
                  rendered' <- reconciliateVirtualDom rendered pos vdom
                  go compRoot' compsPoss' rendered'
      tid <- liftIO myThreadId
      finally
        (do
          -- run for the first time
          comp <- newComponent st0 (\() -> node)
          (comps0, vdom) <- runComp Nothing [] id comp ()
          rendered0 <- renderVirtualDom vdom $ \rendered -> do
            DOM.Node.appendChild_ root (renderedVDomNodeDom (V.nodeBody rendered))
            return rendered
          -- now loop
          go comp comps0 rendered0)
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
