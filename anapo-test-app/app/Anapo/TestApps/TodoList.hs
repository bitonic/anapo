{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.TodoList
  ( TodoState
  , todoComponent
  , todoInit
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad (forM_, when)
import Data.List (partition)
import Data.Monoid ((<>))

import Anapo
import Anapo.Text (Text)
import Anapo.TestApps.Prelude

data TodoItemState = TodoItemState
  { _tisCompleted :: Bool
  , _tisBody :: Text
  } deriving (Eq, Show)
makeLenses ''TodoItemState

todoItemNode :: Node' TodoItemState
todoItemNode = do
  st <- askState
  let aClass = if st^.tisCompleted
        then "list-group-item active"
        else "list-group-item"
  a_
    [ href_ "#"
    , class_ aClass
    , onclick_ $ \_ ev -> do
        preventDefault ev
        dispatch (tisCompleted %= not)
    ]
    (n$ text (st^.tisBody))

data TodoState = TodoState
  { _tsShowCompleted :: Bool
  , _tsTodoElements :: Map.Map Int TodoItemState
  , _tsCurrentText :: Text
  } deriving (Eq, Show)
makeLenses ''TodoState

todoComponent :: Dom' TodoState
todoComponent = do
  st <- askState
  n$ div_ [class_ "row align-items-center"] $ do
    n$ div_ [class_ "col-md-auto"] $ do
      -- submit new item
      simpleTextInput "todo item"
        tsCurrentText
        (dispatch $ do
          curText <- use tsCurrentText
          when (curText /= "") $ do
            let newTodoItem = TodoItemState False curText
            todoEls <- use tsTodoElements
            let itemKey = if Map.null todoEls
                  then 0
                  else fst (Map.findMax todoEls) + 1
            tsCurrentText .= ""
            tsTodoElements.at itemKey .= Just newTodoItem)
        ("Add #" <> tshow (Map.size (st ^. tsTodoElements) + 1))
    -- toggle completed tasks
    n$ div_ [class_ "col"] $ do
      n$ a_
        [ href_ "#"
        , class_ "btn btn-primary"
        , onclick_ $ \_ ev -> do
            preventDefault ev
            dispatch (tsShowCompleted %= not)
        ]
        (n$ if st^.tsShowCompleted
          then "Hide completed tasks"
          else "Show completed tasks")
  -- active / completed todos
  let (done, active) = partition (_tisCompleted . snd) (Map.toAscList (st ^. tsTodoElements))
  let renderItems items =
        n$ div_ [class_ "list-group mx-1 my-2"] $ forM_ items $ \(itemKey, itemState) ->
          zoomT itemState (tsTodoElements . ix itemKey) $
            key (tshow itemKey) todoItemNode
  n$ div_ [class_ "row"] $ do
    n$ div_ [class_ "col"] $ do
      n$ h2_ [class_ "mx-1 my-2"] (n$"Things to do")
      renderItems active
    when (st ^. tsShowCompleted) $ n$ div_ [class_ "col"] $ do
      n$ h2_ [class_ "mx-1 my-2"] (n$"Things already done")
      renderItems done

todoInit :: Monad m => m TodoState
todoInit = return $ TodoState
  { _tsShowCompleted = True
  , _tsTodoElements = Map.fromList
      [ (1, TodoItemState{_tisCompleted = True, _tisBody = "Buy milk"})
      , (2, TodoItemState{_tisCompleted = False, _tisBody = "Learn CT"})
      ]
  , _tsCurrentText = ""
  }
