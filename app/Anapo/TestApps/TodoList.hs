{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.TodoList
  ( TodoState
  , todoComponent
  , todoInit
  ) where

import Control.Lens (makeLenses, over, (^.), set, at)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, when)
import Data.List (partition)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM

import Anapo.Component
import Anapo.TestApps.Prelude

data TodoItemState = TodoItemState
  { _tisCompleted :: Bool
  , _tisBody :: T.Text
  } deriving (Eq, Show)
makeLenses ''TodoItemState

todoItemComponent :: Component TodoItemState (Maybe TodoItemState)
todoItemComponent = do
  st <- askState
  dispatch <- askDispatch
  n$ input_
    (type_ "checkbox")
    (checked_ (st ^. tisCompleted))
    (name_ (st ^. tisBody))
    (onchange_ $ \el ev -> do
      DOM.preventDefault ev
      checked <- DOM.getChecked el
      dispatch (set (traverse . tisCompleted) checked))
  n$ text (st ^. tisBody)

data TodoState = TodoState
  { _tsShowCompleted :: Bool
  , _tsTodoElements :: Map.Map Int TodoItemState
  , _tsCurrentText :: T.Text
  } deriving (Eq, Show)
makeLenses ''TodoState

todoComponent :: Component' TodoState
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
      liftIO (putStrLn "Submitting new item")
      dispatch $ \st' -> if st' ^. tsCurrentText /= ""
        then let
          newTodoItem = TodoItemState False (st' ^. tsCurrentText)
          itemKey = if Map.null (st' ^. tsTodoElements)
            then 0
            else fst (Map.findMax (st' ^. tsTodoElements)) + 1
          in set tsCurrentText "" (over tsTodoElements (Map.insert itemKey newTodoItem) st')
        else st')
    (do
      n$ input_
        (value_ (st ^. tsCurrentText))
        (onchange_ $ \inp _ -> do
          liftIO (putStrLn "Setting new text!")
          txt <- DOM.getValue inp
          dispatch (set tsCurrentText txt))
      n$ button_ $
        n$ text ("Add #" <> tshow (Map.size (st ^. tsTodoElements) + 1)))
  -- active / completed todos
  let (done, active) = partition (_tisCompleted . snd) (Map.toAscList (st ^. tsTodoElements))
  let renderItems items =
        n$ ul_ $ forM_ items $ \(itemKey, itemState) ->
          zoom_ itemState (tsTodoElements . at itemKey) $
            key (tshow itemKey) (li_ todoItemComponent)
  n$ h2_ (n$ "Things to do")
  renderItems active
  when (st ^. tsShowCompleted) $ do
    n$ h2_ (n$ "Things already done")
    renderItems done

todoInit :: ClientM TodoState
todoInit = return $ TodoState
  { _tsShowCompleted = True
  , _tsTodoElements = Map.fromList
      [ (1, TodoItemState{_tisCompleted = False, _tisBody = "Buy milk"})
      , (2, TodoItemState{_tisCompleted = True, _tisBody = "Get a life"})
      ]
  , _tsCurrentText = ""
  }
