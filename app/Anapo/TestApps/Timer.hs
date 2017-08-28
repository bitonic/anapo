{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.Timer (TimerState, timerComponent, timerInit, timerStop) where

import Control.Lens (makeLenses, makePrisms, (^.), set)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Control.Concurrent.Async.Lifted.Safe (Async)
import qualified Control.Concurrent.Async.Lifted.Safe as Async
import Data.Void (Void)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import Anapo
import Anapo.TestApps.Prelude

data Running =
    Stopped
  | Running
      UTCTime -- ^ when it was started
      UTCTime -- ^ the current time
      (Async (ClientM Void)) -- ^ the loop updating the time
makePrisms ''Running

data TimerState = TimerState
  { _tsRunning :: Running
  , _tsTimePassed :: NominalDiffTime
  }
makeLenses ''TimerState

timerComponent :: Component' TimerState
timerComponent = do
  st <- askState
  let timePassed =
        st ^. tsTimePassed +
        (case st ^. tsRunning of
          Stopped -> 0
          Running t0 t1 _ -> diffUTCTime t1 t0)
  n$ h2_ $
    n$ text_ (tshow timePassed)
  dispatch <- askDispatchM
  n$ button_
    (onclick_ (\_ _ -> timerReset dispatch))
    (n$ "Reset")
  n$ button_
    (onclick_ (\_ _ -> timerToggle dispatch))
    (n$ case st ^. tsRunning of
      Running{} -> "Stop"
      Stopped{} -> "Start")

timerInit :: ClientM TimerState
timerInit = return $ TimerState
  { _tsRunning = Stopped
  , _tsTimePassed = 0
  }

timerStop :: TimerState -> ClientM TimerState
timerStop st = case st ^. tsRunning of
  Stopped -> return st
  Running t0 _t1 timer -> do
    t1 <- liftIO getCurrentTime
    Async.cancel timer
    return TimerState
      { _tsRunning = Stopped
      , _tsTimePassed = st ^. tsTimePassed + diffUTCTime t1 t0
      }

timerToggle :: DispatchM TimerState -> ClientM ()
timerToggle disp = disp $ \st -> case st ^. tsRunning of
  Stopped -> do
    t0 <- liftIO getCurrentTime
    timer <- Async.async $ forever $ do
      liftIO (threadDelay (100 * 1000))
      timerBump disp
    return TimerState
      { _tsRunning = Running t0 t0 timer
      , _tsTimePassed = st ^. tsTimePassed
      }
  Running t0 _t1 timer -> do
    t1 <- liftIO getCurrentTime
    Async.cancel timer
    return TimerState
      { _tsRunning = Stopped
      , _tsTimePassed = st ^. tsTimePassed + diffUTCTime t1 t0
      }

timerBump :: DispatchM TimerState -> ClientM ()
timerBump disp = disp $ \st -> case st ^. tsRunning of
  Stopped -> return st
  Running t0 _t1 timer -> do
    t1 <- liftIO getCurrentTime
    return (set tsRunning (Running t0 t1 timer) st)

timerReset :: DispatchM TimerState -> ClientM ()
timerReset disp = disp $ \st -> case st ^. tsRunning of
  Stopped -> timerInit
  Running _t0 _t1 timer -> do
    Async.cancel timer
    timerInit
