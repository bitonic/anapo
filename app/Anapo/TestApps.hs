{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Lens (makeLenses, view, set, (^.))
import Control.Monad (forM_)

import Anapo.Component
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList

import qualified GHCJS.DOM.HTMLSelectElement as DOM

data WhichTestApp =
    Todo
  | Blank
  deriving (Eq, Show, Read)

data TestAppsState = TestAppsState
  { _tasWhich :: WhichTestApp
  , _tasTodo :: TodoState
  } deriving (Eq, Show)
makeLenses ''TestAppsState

testAppsComponent :: Component' TestAppsState
testAppsComponent = do
  dispatch <- askDispatch
  st <- askState
  bootstrapRow $ bootstrapCol $ do
    n$ "Choose an app:"
    n$ select_
      (onchange_ $ \el _ -> do
        whichTxt <- DOM.getValue el
        liftIO (putStrLn ("Setting to " ++ whichTxt))
        dispatch (set tasWhich (read whichTxt)))
      (forM_ [Blank, Todo] $ \which -> do
        n$ option_
          (optionValue_ (tshow which))
          (selected_ (which == st ^. tasWhich))
          (n$ text (tshow which)))
  which <- view tasWhich <$> askState
  case which of
    Blank -> return ()
    Todo -> bootstrapRow (bootstrapCol (zoom' tasTodo todoComponent))

testAppsInit :: ClientM TestAppsState
testAppsInit = TestAppsState
  <$> pure Todo
  <*> todoInit
