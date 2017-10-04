{-# LANGUAGE OverloadedStrings #-}
module Anapo.Loop
  ( componentLoop
  , installComponentBody
  , installComponentBootstrap
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
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM

import qualified Anapo.VDOM as V
import Anapo.Component
import Anapo.Render
import Anapo.Text (Text, pack)
import Anapo.Logging

{-# INLINE timeIt #-}
timeIt :: DOM.JSM a -> DOM.JSM (a, NominalDiffTime)
timeIt m = do
  t0 <- liftIO getCurrentTime
  x <- m
  t1 <- liftIO getCurrentTime
  return (x, diffUTCTime t1 t0)

{-# INLINE componentLoop #-}
componentLoop :: forall state acc.
     (forall a. RegisterThread -> HandleException -> Dispatch state -> (state -> DOM.JSM a) -> DOM.JSM a)
  -> Component (Either SomeException state) state
  -> acc -> (acc -> V.Dom -> DOM.JSM acc)
  -> DOM.JSM acc
  -- ^ returns the final accumulator, when there is nothing left to do.
  -- might never terminate
componentLoop withState vdom !acc0 useDom = do
  -- dispatch channel
  dispatchChan :: Chan (state -> DOM.JSM state) <- liftIO newChan
  let
    get = do
      mbF :: Either BlockedIndefinitelyOnMVar (state -> DOM.JSM state) <- liftIO (tryAsync (readChan dispatchChan))
      case mbF of
        Left{} -> do
          logInfo "got undefinitedly blocked on mvar on dispatch channel, will stop"
          return Nothing
        Right f -> return (Just f)
  let dispatch = writeChan dispatchChan
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
  -- compute state
  withState register handler dispatch $ \st0 -> do
    -- main loop
    let
      go :: Maybe state -> Either SomeException state -> acc -> DOM.JSM acc
      go mbPrevSt !stOrErr !acc = do
        (nodes, vdomDt) <- timeIt (runComponent vdom register handler dispatch mbPrevSt stOrErr)
        DOM.syncPoint
        logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
        acc' <- useDom acc nodes
        case stOrErr of
          Left err -> do
            logError ("Just got exception and rendered, will rethrow: " <> pack (show err))
            liftIO (throwIO err)
          Right st -> do
            -- we are biased towards exceptions since we want
            -- to exit immediately when there is a failure
            fOrErr :: Either SomeException (Maybe (state -> DOM.JSM state)) <- liftIO (Async.race (readMVar excVar) get)
            case fOrErr of
              Left err -> go Nothing (Left err) acc'
              Right Nothing -> do
                logInfo "No state update received, terminating component loop"
                return acc'
              Right (Just f) -> do
                (st', updateDt) <- timeIt (f st)
                logDebug ("State updated (" <> pack (show updateDt) <> "), will re render")
                go (Just st) (Right st') acc'
    tid <- liftIO myThreadId
    finally
      (go Nothing (Right st0) acc0)
      (liftIO (readIORef tidsRef >>= traverse_ (\tid' -> when (tid /= tid') (killThread tid'))))

{-# INLINE installComponentBody #-}
installComponentBody ::
     RenderOptions
  -> (forall a. RegisterThread -> HandleException -> Dispatch state -> (state -> DOM.JSM a) -> DOM.JSM a)
  -> Component (Either SomeException state) state
  -> DOM.JSM ()
installComponentBody ro getSt vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  void $ componentLoop getSt vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc body mbVdom vdom
    return (Just (vdom, evts))

{-# INLINE installComponentBootstrap #-}
installComponentBootstrap ::
     RenderOptions
  -> (forall a. RegisterThread -> HandleException -> Dispatch state -> (state -> DOM.JSM a) -> DOM.JSM a)
  -> Component (Either SomeException state) state
  -> DOM.JSM ()
installComponentBootstrap ro getSt vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  container <- DOM.unsafeCastTo DOM.HTMLDivElement =<< DOM.createElement doc ("div" :: Text)
  DOM.setClassName container ("container" :: Text)
  DOM.appendChild_ body container
  void $ componentLoop getSt vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc container mbVdom vdom
    return (Just (vdom, evts))

