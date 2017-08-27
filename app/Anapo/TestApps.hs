{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Lens (makeLenses, set, (^.), traverseOf)
import Control.Monad (forM_)

import Anapo.Component
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer

import qualified GHCJS.DOM.HTMLSelectElement as DOM

data WhichTestApp =
    Blank
  | Todo
  | Timer
  deriving (Eq, Show, Read)

data TestAppsState = TestAppsState
  { _tasWhich :: WhichTestApp
  , _tasTodo :: TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  }
makeLenses ''TestAppsState

testAppsComponent :: Component' TestAppsState
testAppsComponent = do
  dispatchM <- askDispatchM
  st <- askState
  bootstrapRow $ do
    bootstrapCol $ do
      n$ "Choose an app:"
      n$ select_
        (onchange_ $ \el _ -> do
          newApp <- read <$> DOM.getValue el
          dispatchM $ \st' -> do
            st'' <- if st^.tasStopTimerOnAppChange && newApp /= Timer
              then traverseOf tasTimer timerStop st'
              else return st'
            return (set tasWhich newApp st''))
        (forM_ [Blank, Todo, Timer] $ \which -> do
          n$ option_
            (value_ (tshow which))
            (selected_ (which == st ^. tasWhich))
            (n$ text (tshow which)))
    bootstrapCol $ do
      zoom' tasStopTimerOnAppChange (n$ booleanCheckbox)
      n$ "Stop timer app when changing app"
  case st ^. tasWhich of
    Blank -> return ()
    Todo -> bootstrapRow (bootstrapCol (zoom' tasTodo todoComponent))
    Timer -> bootstrapRow (bootstrapCol (zoom' tasTimer timerComponent))

testAppsInit :: ClientM TestAppsState
testAppsInit = TestAppsState
  <$> pure Todo
  <*> todoInit
  <*> timerInit
  <*> pure False
