{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps (TestAppsState, testAppsComponent, testAppsInit) where

import Control.Lens (makeLenses, view)
import Control.Monad (forM_)

import Anapo.Component
import Anapo.TestApps.Prelude
import Anapo.TestApps.TodoList

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
  n$ select_
    (forM_ [Blank, Todo] $ \which -> do
      n$ option_
        (value_ (tshow which))
        (n$ text (tshow which)))
  which <- view tasWhich <$> askState
  case which of
    Blank -> return ()
    Todo -> zoom' tasTodo todoComponent

testAppsInit :: ClientM TestAppsState
testAppsInit = TestAppsState
  <$> pure Todo
  <*> todoInit
