{-# LANGUAGE OverloadedStrings #-}
module Anapo.Loop
  ( componentLoop
  , installComponentBody
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

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM.Node
import qualified GHCJS.DOM.Document as DOM.Document

import qualified Anapo.VDOM as V
import Anapo.Component
import Anapo.Render
import Anapo.Text (pack)
import Anapo.Logging

{-# INLINE timeIt #-}
timeIt :: DOM.JSM a -> DOM.JSM (a, NominalDiffTime)
timeIt m = do
  t0 <- liftIO getCurrentTime
  x <- m
  t1 <- liftIO getCurrentTime
  return (x, diffUTCTime t1 t0)

{-# INLINE componentLoop #-}
componentLoop :: forall state.
     (forall a. (state -> DOM.JSM a) -> Action state a)
  -> Node (Either SomeException state) state
  -> DOM.Node
  -- ^ where to place the node
  -> DOM.JSM (V.Node RenderedVDomNode)
  -- ^ returns the final rendered node, when there is nothing left to
  -- do. might never terminate
componentLoop withState comp root = do
  -- dispatch channel
  dispatchChan :: Chan (state -> DOM.JSM state) <- liftIO newChan
  let
    get = do
      mbF :: Either BlockedIndefinitelyOnMVar (state -> DOM.JSM state) <- tryAsync (readChan dispatchChan)
      case mbF of
        Left{} -> do
          logInfo "got undefinitedly blocked on mvar on dispatch channel, will stop"
          return Nothing
        Right f -> return (Just f)
  let disp = writeChan dispatchChan
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
  -- helper to run the component
  let runComp mbPrevSt stOrErr = do
        ((_, vdom), vdomDt) <- timeIt (runComponentM comp register handler disp mbPrevSt stOrErr)
        DOM.syncPoint
        logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
        return vdom
  -- compute state
  runAction
    (withState $ \st0 -> do
      -- main loop
      let
        go ::
             state
          -- ^ the previous state
          -> V.Node RenderedVDomNode
          -- ^ the previous rendered node
          -> DOM.JSM (V.Node RenderedVDomNode)
        go st !rendered = do
          -- get the next update or the next exception. we are biased
          -- towards exceptions since we want to exit immediately when
          -- there is a failure.
          fOrErr :: Either SomeException (Maybe (state -> DOM.JSM state)) <- liftIO (Async.race (readMVar excVar) get)
          case fOrErr of
            Left err -> do
              -- if we got an exception, render one last time and shut down
              logError ("Got exception, will render it and rethrow: " <> pack (show err))
              vdom <- runComp (Just st) (Left err)
              void (reconciliateVirtualDom rendered [] vdom)
              logError ("Just got exception and rendered, will rethrow: " <> pack (show err))
              liftIO (throwIO err)
            Right Nothing -> do
              logInfo "No state update received, terminating component loop"
              return rendered
            Right (Just f) -> do
              (st', updateDt) <- timeIt (f st)
              logDebug ("State updated (" <> pack (show updateDt) <> "), will re render")
              vdom <- runComp (Just st) (Right st')
              rendered' <- reconciliateVirtualDom rendered [] vdom
              go st' rendered'
      tid <- liftIO myThreadId
      finally
        (do
          -- run for the first time
          vdom <- runComp Nothing (Right st0)
          rendered0 <- renderVirtualDom vdom $ \rendered -> do
            DOM.Node.appendChild_ root (renderedVDomNodeDom (V.nodeBody rendered))
            return rendered
          -- now loop
          go st0 rendered0)
        (liftIO (readIORef tidsRef >>= traverse_ (\tid' -> when (tid /= tid') (killThread tid')))))
    register handler disp

{-# INLINE installComponentBody #-}
installComponentBody ::
     (forall a. (state -> DOM.JSM a) -> Action state a)
  -> Node (Either SomeException state) state
  -> DOM.JSM ()
installComponentBody getSt vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.Document.getBodyUnchecked doc
  void $ componentLoop getSt vdom0 (DOM.toNode body)
