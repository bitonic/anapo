{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps (TestAppsStateOrError, testAppsError, testAppsComponent, testAppsWith) where

import Control.Lens (makeLenses, set, (^.), makePrisms)
import Control.Monad (forM_, when)
import Data.Typeable (Typeable)
import Control.Exception.Safe (bracket)
import GHCJS.Marshal (fromJSVal)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad.Reader (ask)

import Anapo
import Anapo.Text (Text)
import qualified Anapo.Text as T
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList
import Anapo.TestApps.Timer
import Anapo.TestApps.YouTube
import Anapo.TestApps.Bump
import Anapo.TestApps.KeyedList
import Anapo.TestApps.ComponentDifferentNodes
import Anapo.TestApps.RawHtml

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
  | Bumps
  | KeyedList
  | DifferentNodes
  | RawHtml
  deriving (Eq, Show, Read, Enum, Bounded, Typeable)

testAppToPath :: WhichTestApp -> Text
testAppToPath = \case
  Blank -> "/blank"
  Todo -> "/todo"
  Timer -> "/timer"
  YouTube -> "/youTube"
  Bumps -> "/bumps"
  KeyedList -> "/keyedList"
  DifferentNodes -> "/differentNodes"
  RawHtml -> "/rawHtml"

testAppFromPath :: Text -> Maybe WhichTestApp
testAppFromPath = \case
  "/blank" -> Just Blank
  "/todo" -> Just Todo
  "/timer" -> Just Timer
  "/youTube" -> Just YouTube
  "/bumps" -> Just Bumps
  "/keyedList" -> Just KeyedList
  "/differentNodes" -> Just DifferentNodes
  "/rawHtml" -> Just RawHtml
  "/" -> Just Todo
  _ -> Nothing

allTestApps :: [WhichTestApp]
allTestApps = [minBound..maxBound]

data TestAppsState = TestAppsState
  { _tasApp :: WhichTestApp
  , _tasFirstApp :: WhichTestApp
  , _tasTodo :: Component () () TodoState
  , _tasTimer :: TimerState
  , _tasStopTimerOnAppChange :: Bool
  , _tasYouTube :: YouTubeState
  , _tasBumps :: Component () () BumpsState
  , _tasKeyedList :: Component () () KeyedListState
  , _tasDifferentNodes :: Component () () Bool
  , _tasRawHtml :: Component () () Bool
  }
makeLenses ''TestAppsState

data TestAppsStateOrError =
    TASOEError Text
  | TASOEOk TestAppsState
makePrisms ''TestAppsStateOrError

changeToApp :: MonadAction a TestAppsState m => Bool -> Maybe WhichTestApp -> m ()
changeToApp pushHistory mbApp =
  dispatch $ do
    st' <- get
    let app = fromMaybe (st'^.tasFirstApp) mbApp
    when pushHistory $ do
      let appShown = tshow app
      window <- DOM.currentWindowUnchecked
      history <- DOM.Window.getHistory window
      DOM.History.pushState history appShown ("" :: Text) (Just (testAppToPath app))
    st'' <- if st'^.tasStopTimerOnAppChange && app /= Timer
      then tasTimer timerStop st'
      else return st'
    put (set tasApp app st'')

testAppsError :: Text -> Node a state
testAppsError err =
  div_ [class_ "container"] $ do
    n$ div_ [class_ "m-2 alert alert-danger"] (n$ text err)

testAppsComponent ::
  Node () TestAppsStateOrError
testAppsComponent = do
  stoe <- ask
  case stoe of
    TASOEError err -> testAppsError err
    TASOEOk st -> div_ [class_ "container"] $ zoomT st _TASOEOk $ do
      n$ div_ [class_ "row m-2 align-items-center"] $ do
        n$ div_ [class_ "col col-md-auto"] $ do
          n$ ul_ [class_ "nav nav-pills"] $ forM_ allTestApps $ \app -> do
            let aClass = if app == st^.tasApp
                  then "nav-link active"
                  else "nav-link"
            n$ li_ [class_ "nav-item"] $ n$ a_
              [ class_ aClass
              , href_ (testAppToPath app)
              , onclick_ $ \_ ev -> do
                  DOM.preventDefault ev
                  changeToApp True (Just app)
              ]
              (n$ text (tshow app))
        n$ div_ [class_ "col"] $ zoomL tasStopTimerOnAppChange $
          n$ div_ [class_ "form-check"] $
            n$ label_ [class_ "form-check-label"] $ do
              n$ booleanCheckbox
              n$ "Stop timer app when changing app"
      n$ div_ [class_ "row m-2"] $ n$ div_ [class_ "col"] $ case st^.tasApp of
        Blank -> return ()
        Todo -> n$ componentL tasTodo ()
        Timer -> zoomL tasTimer timerComponent
        YouTube -> zoomL tasYouTube youTubeComponent
        Bumps -> n$ componentL tasBumps ()
        KeyedList -> n$ componentL tasKeyedList ()
        DifferentNodes -> n$ componentL tasDifferentNodes ()
        RawHtml -> n$ componentL tasRawHtml ()

testAppsWith ::
     (TestAppsStateOrError -> Action () TestAppsStateOrError a)
  -> Action () TestAppsStateOrError a
testAppsWith cont = do
  path <- liftJSM (DOM.getPathname =<< DOM.getLocation =<< DOM.currentWindowUnchecked)
  case testAppFromPath path of
    Nothing -> cont (TASOEError ("No app at location " <> path))
    Just app -> do
      window <- liftJSM DOM.currentWindowUnchecked
      u <- askUnliftIO
      liftIO $ bracket
        (unliftIO u $ do
          listener <- liftJSM $ DOM.newListener $ do
            ev <- ask
            st <- DOM.PopStateEvent.getState ev
            mbApp <- liftJSM (fmap (read . T.unpack) <$> fromJSVal st)
            liftIO (unliftIO u (actZoom _TASOEOk (changeToApp False mbApp)))
          liftJSM (DOM.addListener window DOM.popState listener False)
          return listener)
        (\listener -> unliftIO u (liftJSM (DOM.removeListener window DOM.popState listener False)))
        (\_ -> unliftIO u $ do
            todo <- actZoom (_TASOEOk.tasTodo.compState) todoInit
            bumps <- actZoom (_TASOEOk.tasBumps.compState) bumpsInit
            keyedList <- actZoom (_TASOEOk.tasKeyedList.compState) keyedListInit
            st <- TestAppsState
              <$> pure app
              <*> pure app
              <*> newComponent_ todo (\() -> todoComponent)
              <*> timerInit
              <*> pure False
              <*> youTubeInit "3yQObSCXyoo"
              <*> newComponent_ bumps (\() -> bumpsNode)
              <*> newComponent_ keyedList (\() -> keyedListComponent)
              <*> componentDifferentNodesInit
              <*> newComponent_ False (\() -> rawHtmlComponent)
            cont (TASOEOk st))
