{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Lens (makeLenses, set, (^.), traverseOf)
import Control.Monad (forM_)

import Anapo
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.HogJowls

import qualified GHCJS.DOM.HTMLSelectElement as DOM

data WhichTestApp =
    Blank
  | Todo
  | Timer
  | HogJowls
  deriving (Eq, Show, Read)

data TestAppsState = TestAppsState
  { _tasWhich :: WhichTestApp
  , _tasTodo :: TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  , _tasHogJowls :: HogJowlsState
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
        (forM_ [Blank, Todo, Timer, HogJowls] $ \which -> do
          n$ option_
            (value_ (tshow which))
            (selected_ (which == st ^. tasWhich))
            (n$ text_ (tshow which)))
    bootstrapCol $ do
      zoom' tasStopTimerOnAppChange (n$ booleanCheckbox)
      n$ "Stop timer app when changing app"
  bootstrapRow $ bootstrapCol $ case st^.tasWhich of
    Blank -> return ()
    Todo -> zoom' tasTodo todoComponent
    Timer -> zoom' tasTimer timerComponent
    HogJowls -> zoom' tasHogJowls hogJowlsComponent

testAppsInit :: ClientM TestAppsState
testAppsInit = TestAppsState
  <$> pure Todo
  <*> todoInit
  <*> timerInit
  <*> pure False
  <*> hogJowlsInit
