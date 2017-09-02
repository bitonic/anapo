{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Lens (makeLenses, set, (^.))
import Control.Monad (forM_)

import Anapo
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.YouTube
import Anapo.TestApps.SlowRequest

import qualified GHCJS.DOM.Event as DOM

data WhichTestApp =
    Blank
  | Todo
  | Timer
  | YouTube
  | SlowRequest
  deriving (Eq, Show, Read, Enum, Bounded)

allTestApps :: [WhichTestApp]
allTestApps = [minBound..maxBound]

data TestAppsState = TestAppsState
  { _tasWhich :: WhichTestApp
  , _tasTodo :: TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  , _tasYouTube :: YouTubeState
  , _tasSlowRequest :: SlowRequestState
  }
makeLenses ''TestAppsState

testAppsComponent :: Component' TestAppsState
testAppsComponent = do
  dispatchM <- askDispatchM
  st <- askState
  n$ div_ (class_ "row m-2 align-items-center") $ do
    n$ div_ (class_ "col col-md-auto") $ do
      n$ ul_ (class_ "nav nav-pills") $ forM_ allTestApps $ \app -> do
        let aClass = if app == st^.tasWhich
              then "nav-link active"
              else "nav-link"
        n$ li_ (class_ "nav-item") $ n$ a_
          (class_ aClass)
          (href_ "#")
          (onclick_ $ \_ ev -> do
            DOM.preventDefault ev
            dispatchM $ \st' -> do
              st'' <- if st^.tasStopTimerOnAppChange && app /= Timer
                then tasTimer timerStop st'
                else return st'
              return (set tasWhich app st''))
          (n$ text (tshow app))
    bootstrapCol $ zoom' tasStopTimerOnAppChange $
      n$ div_ (class_ "form-check") $
        n$ label_ (class_ "form-check-label") $ do
          n$ booleanCheckbox
          n$ "Stop timer app when changing app"
  n$ div_ (class_ "row m-2") $ bootstrapCol $ case st^.tasWhich of
    Blank -> return ()
    Todo -> zoom' tasTodo todoComponent
    Timer -> zoom' tasTimer timerComponent
    YouTube -> zoom' tasYouTube youTubeComponent
    SlowRequest -> zoom' tasSlowRequest slowRequestComponent

testAppsInit :: ClientM TestAppsState
testAppsInit = TestAppsState
  <$> pure Todo
  <*> todoInit
  <*> timerInit
  <*> pure False
  <*> youTubeInit "Hah4iGqh7GY"
  <*> slowRequestInit
