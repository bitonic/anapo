{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.TodoList
  ( TodoState
  , todoComponent
  , todoInit
  ) where

import Control.Lens (makeLenses, over, (^.), _Just, set, at)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad (unless, forM_, when)
import Data.List (partition)
import Data.Monoid ((<>))

import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM

import Anapo.Component
import Anapo.TestApps.Prelude

data TodoItemState = TodoItemState
  { _tisCompleted :: Bool
  , _tisBody :: T.Text
  } deriving (Eq, Show)
makeLenses ''TodoItemState

todoItemComponent :: Component (Maybe TodoItemState)
todoItemComponent = do
  mbSt <- askState
  case mbSt of
    Nothing -> fail "item component state not present!"
    Just st -> do
      dispatch <- askDispatch
      unless (st ^. tisCompleted) $
        n$ input_
          (type_ "checkbox")
          (onchange_ (\_ _ -> dispatch (over (_Just . tisCompleted) not)))
      n$ text (st ^. tisBody)

data TodoState = TodoState
  { _tsShowCompleted :: Bool
  , _tsTodoElements :: Map.Map Int TodoItemState
  , _tsCurrentText :: T.Text
  } deriving (Eq, Show)
makeLenses ''TodoState

todoComponent :: Component TodoState
todoComponent = do
  st <- askState
  dispatch <- askDispatch
  -- toggle completed tasks
  n$ a_
    (href_ "#")
    (onclick_ $ \_ ev -> do
      DOM.preventDefault ev
      dispatch (over tsShowCompleted not))
    (n$ if st ^. tsShowCompleted
      then "Hide completed tasks"
      else "Show completed tasks")
  -- submit new item
  n$ form_
    (onsubmit_ $ \_ ev -> do
      DOM.preventDefault ev
      dispatch $ \st' -> let
        newTodoItem = TodoItemState False (st' ^. tsCurrentText)
        itemKey = if Map.null (st' ^. tsTodoElements)
          then 0
          else fst (Map.findMax (st' ^. tsTodoElements)) + 1
        in set tsCurrentText "" (over tsTodoElements (Map.insert itemKey newTodoItem) st'))
    (do
      n$ input_
        (value_ (st ^. tsCurrentText))
        (onchange_ $ \inp _ -> do
          txt <- DOM.getValue inp
          dispatch (set tsCurrentText txt))
      n$ button_ $
        n$ text ("Add #" <> tshow (Map.size (st ^. tsTodoElements) + 1)))
  -- active / completed todos
  let (done, active) = partition (_tisCompleted . snd) (Map.toAscList (st ^. tsTodoElements))
  let renderItems items =
        n$ ul_ $ forM_ items $ \itemKey ->
          zoom (tsTodoElements . at itemKey) (key (tshow itemKey) (li_ todoItemComponent))
  n$ h2_ (n$ "Things to do")
  renderItems (map fst active)
  when (st ^. tsShowCompleted) $ do
    n$ h2_ (n$ "Things already done")
    renderItems (map fst done)

todoInit :: ClientM TodoState
todoInit = return $ TodoState
  { _tsShowCompleted = True
  , _tsTodoElements = Map.fromList
      [ (1, TodoItemState{_tisCompleted = True, _tisBody = "Buy milk"})
      , (2, TodoItemState{_tisCompleted = False, _tisBody = "Get a life"})
      ]
  , _tsCurrentText = ""
  }
