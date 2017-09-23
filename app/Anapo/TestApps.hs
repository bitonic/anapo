{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsStateOrError, testAppsComponent, testAppsWith) where

import Control.Lens (makeLenses, set, (^.), makePrisms)
import Control.Monad (forM_, when)
import Data.JSString
import Data.Typeable (Typeable)
import GHCJS.Foreign.Callback
import Control.Exception.Safe (bracket)
import GHCJS.Types (jsval)
import qualified Data.JSString as JSS
import GHCJS.Marshal.Pure (pFromJSVal)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Anapo
import Anapo.History
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.YouTube
import Anapo.TestApps.SlowRequest

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.Window as DOM
import qualified GHCJS.DOM.Location as DOM

data WhichTestApp =
    Blank
  | Todo
  | Timer
  | YouTube
  | SlowRequest
  deriving (Eq, Show, Read, Enum, Bounded, Typeable)

testAppToPath :: WhichTestApp -> JSString
testAppToPath = \case
  Blank -> "/blank"
  Todo -> "/todo"
  Timer -> "/timer"
  YouTube -> "/youTube"
  SlowRequest -> "/slowRequest"

testAppFromPath :: JSString -> Maybe WhichTestApp
testAppFromPath = \case
  "/blank" -> Just Blank
  "/todo" -> Just Todo
  "/timer" -> Just Timer
  "/youTube" -> Just YouTube
  "/slowRequest" -> Just SlowRequest
  "/" -> Just Todo
  _ -> Nothing

allTestApps :: [WhichTestApp]
allTestApps = [minBound..maxBound]

data TestAppsState = TestAppsState
  { _tasApp :: WhichTestApp
  , _tasFirstApp :: WhichTestApp
  , _tasTodo :: TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  , _tasYouTube :: YouTubeState
  , _tasSlowRequest :: SlowRequestState
  }
makeLenses ''TestAppsState

data TestAppsStateOrError =
    TASOEError JSString
  | TASOEOk TestAppsState
makePrisms ''TestAppsStateOrError

changeToApp :: Bool -> DispatchM TestAppsState -> Maybe WhichTestApp -> ClientM ()
changeToApp pushHistory dispatchM mbApp =
  dispatchM $ \st' -> do
    let app = fromMaybe (st'^.tasFirstApp) mbApp
    when pushHistory $ do
      let appShown = jsshow app
      historyPushState (jsval appShown) "" (testAppToPath app)
    st'' <- if st'^.tasStopTimerOnAppChange && app /= Timer
      then tasTimer timerStop st'
      else return st'
    return (set tasApp app st'')

testAppsComponent :: Component' TestAppsStateOrError
testAppsComponent = do
  stoe <- askState
  case stoe of
    TASOEError err -> n$ div_ (class_ "m-2 alert alert-danger") (n$ text err)
    TASOEOk st -> zoomT st _TASOEOk $ do
      dispatchM <- askDispatchM
      n$ div_ (class_ "row m-2 align-items-center") $ do
        n$ div_ (class_ "col col-md-auto") $ do
          n$ ul_ (class_ "nav nav-pills") $ forM_ allTestApps $ \app -> do
            let aClass = if app == st^.tasApp
                  then "nav-link active"
                  else "nav-link"
            n$ li_ (class_ "nav-item") $ n$ a_
              (class_ aClass)
              (href_ (testAppToPath app))
              (onclick_ $ \_ ev -> do
                DOM.preventDefault ev
                changeToApp True dispatchM (Just app))
              (n$ text (jsshow app))
        bootstrapCol $ zoomL tasStopTimerOnAppChange $
          n$ div_ (class_ "form-check") $
            n$ label_ (class_ "form-check-label") $ do
              n$ booleanCheckbox
              n$ "Stop timer app when changing app"
      n$ div_ (class_ "row m-2") $ bootstrapCol $ case st^.tasApp of
        Blank -> return ()
        Todo -> zoomL tasTodo todoComponent
        Timer -> zoomL tasTimer timerComponent
        YouTube -> zoomL tasYouTube youTubeComponent
        SlowRequest -> zoomL tasSlowRequest slowRequestComponent

testAppsWith :: DispatchM TestAppsStateOrError -> (TestAppsStateOrError -> ClientM ()) -> ClientM ()
testAppsWith dispatch cont = do
  path <- DOM.getPathname =<< DOM.getLocation =<< DOM.currentWindowUnchecked
  case testAppFromPath path of
    Nothing -> cont (TASOEError ("No app at location " <> path))
    Just app -> do
      bracket
        (asyncCallback1 $ \st -> do
          let mbApp = read . JSS.unpack <$> pFromJSVal st
          changeToApp False (dispatch . _TASOEOk) mbApp)
        (\callback -> do
          historyRemoveOnPopState
          releaseCallback callback)
        (\callback -> do
            historySetOnPopState callback
            st <- TestAppsState
              <$> pure app
              <*> pure app
              <*> todoInit
              <*> timerInit
              <*> pure False
              <*> youTubeInit "Hah4iGqh7GY"
              <*> slowRequestInit
            cont (TASOEOk st))
