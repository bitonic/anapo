{-# LANGUAGE OverloadedStrings #-}
-- Module to create "collected" threads. If the 'CollectedThreadId'
-- object returned by 'forkCollected' is garbage collected, the forked
-- thread will be terminated. in other words, the thread will be alive
-- as long as somebody is holding a reference to it.
module Anapo.CollectedThread
  ( CollectedThreadId
  , forkCollected
  , killCollected
  , collectedThreadId
  ) where

import Prelude
import Control.Exception.Safe (uninterruptibleMask)
import Control.Concurrent (ThreadId, killThread)
import Data.IORef (mkWeakIORef, IORef, newIORef, readIORef)
import Control.Monad (void)
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO.Unsafe (unsafePerformIO)

import Anapo.Text (pack)
import Anapo.Logging
import Anapo

newtype CollectedThreadId = CollectedThreadId { _unCollectedThreadId :: IORef (ThreadId) }

instance Show CollectedThreadId where
  showsPrec d (CollectedThreadId ref) = unsafePerformIO $ do
    tid <- readIORef ref
    return (showString "CollectedThreadId " . showsPrec d tid)

instance Eq CollectedThreadId where
  CollectedThreadId ref1 == CollectedThreadId ref2 = unsafePerformIO $ do
    tid1 <- readIORef ref1
    tid2 <- readIORef ref2
    return (tid1 == tid2)

{-# INLINE forkCollected #-}
forkCollected :: MonadAction context state m => Action context state () -> m CollectedThreadId
forkCollected m = liftAction $ do
  -- we use an IORef because we do not have a function that attaches
  -- a finalizer on the primitive ThreadId#, and when i attach it
  -- to the box (ThreadId) it seems to lead to early finalization
  -- sometimes. TODO try to write a function like `mkWeakThreadId`
  -- but with finalizer.
  -- see also
  -- <https://gist.github.com/nh2/2b27a2bb17a7e1926ecb#file-remotevalues-hs-L309>
  u <- askUnliftIO
  ctid <- liftIO $ uninterruptibleMask $ \restore -> do
    tid <- unliftIO u (actFork (liftIO (restore (unliftIO u m))))
    ref <- liftIO (newIORef tid)
    void $ liftIO $ mkWeakIORef ref $ do
      logDebug ("Killing linked thread " <> pack (show tid))
      killThread tid
    return (CollectedThreadId ref)
  tid <- collectedThreadId ctid
  logDebug ("Spawned linked thread " <> pack (show tid))
  return ctid

{-# INLINE killCollected #-}
killCollected :: (MonadIO m) => CollectedThreadId -> m ()
killCollected ctid = liftIO (killThread =<< collectedThreadId ctid)

{-# INLINE collectedThreadId #-}
collectedThreadId :: MonadIO m => CollectedThreadId -> m ThreadId
collectedThreadId (CollectedThreadId ref) = liftIO (readIORef ref)
