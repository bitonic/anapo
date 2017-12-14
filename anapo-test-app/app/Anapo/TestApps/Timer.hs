{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.Timer (TimerState, timerComponent, timerInit, timerStop) where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Control.Monad.IO.Class (MonadIO(..))

import Anapo
import qualified Anapo.Text as T
import Anapo.CollectedThread
import Anapo.TestApps.Prelude

data Running =
    Stopped
  | Running
      UTCTime -- ^ when it was started
      UTCTime -- ^ the current time
      CollectedThreadId -- ^ the loop updating the time
makePrisms ''Running

data TimerState = TimerState
  { _tsRunning :: Running
  , _tsTimePassed :: NominalDiffTime
  }
makeLenses ''TimerState

timerComponent :: Dom TimerState
timerComponent = do
  st <- ask
  let timePassed =
        st ^. tsTimePassed +
        (case st ^. tsRunning of
          Stopped -> 0
          Running t0 t1 _ -> diffUTCTime t1 t0)
  n$ h2_ [class_ "mx-1"] $
    n$ text (T.pack (printf "%0.2fs" (realToFrac timePassed :: Double)))
  n$ button_
    [ type_ "button"
    , class_ "mx-1 btn btn-primary"
    , onclick_ (\_ _ -> timerReset)
    ]
    (n$ "Reset")
  n$ button_
    [ type_ "button"
    , class_ $ case st ^. tsRunning of
        Running{} -> "mx-1 btn btn-danger"
        Stopped{} -> "mx-1 btn btn-success"
    , onclick_ (\_ _ -> timerToggle)
    ]
    (n$ case st ^. tsRunning of
      Running{} -> "Stop"
      Stopped{} -> "Start")

timerInit :: Monad m => m TimerState
timerInit = return $ TimerState
  { _tsRunning = Stopped
  , _tsTimePassed = 0
  }

timerStop :: MonadIO m => TimerState -> m TimerState
timerStop st = case st ^. tsRunning of
  Stopped -> return st
  Running t0 _t1 timer -> do
    t1 <- liftIO getCurrentTime
    liftIO (killCollected timer)
    return TimerState
      { _tsRunning = Stopped
      , _tsTimePassed = st ^. tsTimePassed + diffUTCTime t1 t0
      }

timerToggle :: Action TimerState ()
timerToggle = dispatch $ do
  running <- use tsRunning
  passed <- use tsTimePassed
  case running of
    Stopped -> do
      t0 <- liftIO getCurrentTime
      timer <- forkCollected $ forever $ do
        liftIO (threadDelay (100 * 1000))
        timerBump
      put TimerState
        { _tsRunning = Running t0 t0 timer
        , _tsTimePassed = passed
        }
    Running t0 _t1 timer -> do
      t1 <- liftIO getCurrentTime
      killCollected timer
      put TimerState
        { _tsRunning = Stopped
        , _tsTimePassed = passed + diffUTCTime t1 t0
        }

timerBump :: Action TimerState ()
timerBump = dispatch $ do
  running <- use tsRunning
  case running of
    Stopped -> return ()
    Running t0 _t1 timer -> do
      t1 <- liftIO getCurrentTime
      tsRunning .= Running t0 t1 timer

timerReset :: Action TimerState ()
timerReset = dispatch $ do
  running <- use tsRunning
  case running of
    Stopped -> put =<< timerInit
    Running _t0 _t1 timer -> do
      killCollected timer
      put =<< timerInit
