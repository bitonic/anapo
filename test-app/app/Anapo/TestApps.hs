{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsStateOrError, testAppsComponent, testAppsWith) where

import Control.Lens (makeLenses, set, (^.), makePrisms)
import Control.Monad (forM_, when)
import Data.JSString
import Data.Typeable (Typeable)
import Control.Exception.Safe (bracket)
import qualified Data.JSString as JSS
import GHCJS.Marshal.Pure (pFromJSVal)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Anapo
import Anapo.Text (Text)
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.YouTube

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.WindowEventHandlers as DOM (popState)
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.Window as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.EventM as DOM (newListener, addListener, removeListener)
import qualified GHCJS.DOM.PopStateEvent as DOM.PopStateEvent
import qualified GHCJS.DOM.History as DOM.History
import qualified GHCJS.DOM.Window as DOM.Window

data WhichTestApp =
    Blank
  | Todo
  | Timer
  | YouTube
  deriving (Eq, Show, Read, Enum, Bounded, Typeable)

testAppToPath :: WhichTestApp -> JSString
testAppToPath = \case
  Blank -> "/blank"
  Todo -> "/todo"
  Timer -> "/timer"
  YouTube -> "/youTube"

testAppFromPath :: JSString -> Maybe WhichTestApp
testAppFromPath = \case
  "/blank" -> Just Blank
  "/todo" -> Just Todo
  "/timer" -> Just Timer
  "/youTube" -> Just YouTube
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
  }
makeLenses ''TestAppsState

data TestAppsStateOrError =
    TASOEError JSString
  | TASOEOk TestAppsState
makePrisms ''TestAppsStateOrError

changeToApp :: Bool -> Dispatch TestAppsState -> Maybe WhichTestApp -> JSM ()
changeToApp pushHistory dispatchM mbApp =
  dispatchM $ \st' -> do
    let app = fromMaybe (st'^.tasFirstApp) mbApp
    when pushHistory $ do
      let appShown = jsshow app
      window <- DOM.currentWindowUnchecked
      history <- DOM.Window.getHistory window
      DOM.History.pushState history appShown ("" :: Text) (Just (testAppToPath app))
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
      dispatch <- askDispatch
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
                changeToApp True dispatch (Just app))
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

testAppsWith :: Dispatch TestAppsStateOrError -> (TestAppsStateOrError -> JSM ()) -> JSM ()
testAppsWith dispatch cont = do
  path <- DOM.getPathname =<< DOM.getLocation =<< DOM.currentWindowUnchecked
  case testAppFromPath path of
    Nothing -> cont (TASOEError ("No app at location " <> path))
    Just app -> do
      window <- DOM.currentWindowUnchecked
      bracket
        (do
          listener <- DOM.newListener $ do
            ev <- ask
            st <- DOM.PopStateEvent.getState ev
            let mbApp = read . JSS.unpack <$> pFromJSVal st
            lift (changeToApp False (dispatch . _TASOEOk) mbApp)
          DOM.addListener window DOM.popState listener False
          return listener)
        (\listener -> DOM.removeListener window DOM.popState listener False)
        (\_ -> do
            st <- TestAppsState
              <$> pure app
              <*> pure app
              <*> todoInit
              <*> timerInit
              <*> pure False
              <*> youTubeInit "Hah4iGqh7GY"
            cont (TASOEOk st))
