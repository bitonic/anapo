{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.Timer (TimerState, timerComponent, timerInit, timerStop) where

import Control.Lens (makeLenses, makePrisms, (^.), set)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Data.Void (Void)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Control.Monad.IO.Class (MonadIO(..))

import Anapo
import qualified Anapo.Text as T
import Anapo.TestApps.Prelude

data Running =
    Stopped
  | Running
      UTCTime -- ^ when it was started
      UTCTime -- ^ the current time
      (Async (JSM Void)) -- ^ the loop updating the time
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
  n$ h2_ (class_ "mx-1") $
    n$ text (T.pack (printf "%0.2fs" (realToFrac timePassed :: Double)))
  dispatch <- askDispatch
  n$ button_
    (type_ "button")
    (class_ "mx-1 btn btn-primary")
    (onclick_ (\_ _ -> liftIO (timerReset dispatch)))
    (n$ "Reset")
  n$ button_
    (type_ "button")
    (class_ $ case st ^. tsRunning of
      Running{} -> "mx-1 btn btn-danger"
      Stopped{} -> "mx-1 btn btn-success")
    (onclick_ (\_ _ -> liftIO (timerToggle dispatch)))
    (n$ case st ^. tsRunning of
      Running{} -> "Stop"
      Stopped{} -> "Start")

timerInit :: MonadIO m => m TimerState
timerInit = return $ TimerState
  { _tsRunning = Stopped
  , _tsTimePassed = 0
  }

timerStop :: TimerState -> JSM TimerState
timerStop st = case st ^. tsRunning of
  Stopped -> return st
  Running t0 _t1 timer -> do
    t1 <- liftIO getCurrentTime
    liftIO (Async.cancel timer)
    return TimerState
      { _tsRunning = Stopped
      , _tsTimePassed = st ^. tsTimePassed + diffUTCTime t1 t0
      }

timerToggle :: Dispatch TimerState -> IO ()
timerToggle disp = disp $ \st -> liftIO $ case st ^. tsRunning of
  Stopped -> do
    t0 <- getCurrentTime
    timer <- Async.async $ forever $ do
      threadDelay (100 * 1000)
      timerBump disp
    return TimerState
      { _tsRunning = Running t0 t0 timer
      , _tsTimePassed = st ^. tsTimePassed
      }
  Running t0 _t1 timer -> do
    t1 <- getCurrentTime
    Async.cancel timer
    return TimerState
      { _tsRunning = Stopped
      , _tsTimePassed = st ^. tsTimePassed + diffUTCTime t1 t0
      }

timerBump :: Dispatch TimerState -> IO ()
timerBump disp = disp $ \st -> liftIO $ case st ^. tsRunning of
  Stopped -> return st
  Running t0 _t1 timer -> do
    t1 <- getCurrentTime
    return (set tsRunning (Running t0 t1 timer) st)

timerReset :: Dispatch TimerState -> IO ()
timerReset disp = disp $ \st -> liftIO $ case st ^. tsRunning of
  Stopped -> timerInit
  Running _t0 _t1 timer -> do
    Async.cancel timer
    timerInit
