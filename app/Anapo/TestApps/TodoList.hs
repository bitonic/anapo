{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.TodoList
  ( TodoState
  , todoComponent
  , todoInit
  ) where

import Control.Lens (makeLenses, over, (^.), set, ix, (%=))
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, when)
import Data.List (partition)
import Data.Monoid ((<>))
import Data.JSString (JSString)

import qualified GHCJS.DOM.Event as DOM

import Anapo
import Anapo.TestApps.Prelude

data TodoItemState = TodoItemState
  { _tisCompleted :: Bool
  , _tisBody :: JSString
  } deriving (Eq, Show)
makeLenses ''TodoItemState

todoItemNode :: Node' HTMLAnchorElement TodoItemState
todoItemNode = do
  st <- askState
  dispatch <- askDispatch
  let aClass = if st^.tisCompleted
        then "list-group-item active"
        else "list-group-item"
  a_
    (href_ "#")
    (class_ aClass)
    (onclick_ $ \_ ev -> DOM.preventDefault ev >> runDispatch dispatch (tisCompleted %= not))
    (n$ text (st^.tisBody))

data TodoState = TodoState
  { _tsShowCompleted :: Bool
  , _tsTodoElements :: Map.Map Int TodoItemState
  , _tsCurrentText :: JSString
  } deriving (Eq, Show)
makeLenses ''TodoState

todoComponent :: Component' TodoState
todoComponent = do
  st <- askState
  dispatch <- askDispatch
  n$ div_ (class_ "row align-items-center") $ do
    n$ div_ (class_ "col-md-auto") $ do
      -- submit new item
      zoomL tsCurrentText $ simpleTextInput "todo item"
        (dispatch $ \st' -> return $ if st' ^. tsCurrentText /= ""
            then let
              newTodoItem = TodoItemState False (st' ^. tsCurrentText)
              itemKey = if Map.null (st' ^. tsTodoElements)
                then 0
                else fst (Map.findMax (st' ^. tsTodoElements)) + 1
              in set tsCurrentText "" (over tsTodoElements (Map.insert itemKey newTodoItem) st')
            else st')
        ("Add #" <> jsshow (Map.size (st ^. tsTodoElements) + 1))
    -- toggle completed tasks
    n$ div_ (class_ "col") $ do
      n$ a_
        (href_ "#")
        (class_ "btn btn-primary")
        (onclick_ $ \_ ev -> do
          DOM.preventDefault ev
          runDispatch dispatch (tsShowCompleted %= not))
        (n$ if st^.tsShowCompleted
          then "Hide completed tasks"
          else "Show completed tasks")
  -- active / completed todos
  let (done, active) = partition (_tisCompleted . snd) (Map.toAscList (st ^. tsTodoElements))
  let renderItems items =
        n$ div_ (class_ "list-group mx-1 my-2") $ forM_ items $ \(itemKey, itemState) ->
          zoomT itemState (tsTodoElements . ix itemKey) $
            key (jsshow itemKey) todoItemNode
  bootstrapRow $ do
    bootstrapCol $ do
      n$ h2_ (class_ "mx-1 my-2") (n$ "Things to do")
      renderItems active
    when (st ^. tsShowCompleted) $ bootstrapCol $ do
      n$ h2_ (class_ "mx-1 my-2") (n$ "Things already done")
      renderItems done

todoInit :: ClientM TodoState
todoInit = return $ TodoState
  { _tsShowCompleted = True
  , _tsTodoElements = Map.fromList
      [ (1, TodoItemState{_tisCompleted = True, _tisBody = "Buy milk"})
      , (2, TodoItemState{_tisCompleted = False, _tisBody = "Learn CT"})
      ]
  , _tsCurrentText = ""
  }
