{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.KeyedList (KeyedListState, keyedListInit, keyedListComponent) where

import qualified Data.Map.Strict as MS
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
import Control.Lens (_1, _2, ix, (+=))
import Text.Read (readMaybe)
import qualified Data.Foldable as F
import Control.Monad (when)

import Anapo
import Anapo.TestApps.Prelude

data KeyedListState = KeyedListState
  { _klsElements :: MS.Map Int (Int, Text)
  , _klsInput :: (Int, Text)
  , _klsCounter :: Int
  }
makeLenses ''KeyedListState

keyedListInit :: Monad m => m KeyedListState
keyedListInit = return (KeyedListState mempty (0, "") 0)

keyedListComponent :: Node KeyedListState
keyedListComponent = do
  st <- ask
  div_ [] $ do
    n$ div_ [class_ "row"] $ n$ div_ [class_ "col"] $ do
      n$ form_
        [ class_ "form-inline mb-2 mt-2"
        , onsubmit_ $ \_ ev -> do
            preventDefault ev
            dispatch $ do
              (prio, txt) <- use klsInput
              when (txt /= "") $ do
                count <- use klsCounter
                klsElements %= MS.insert count (prio, txt)
                klsCounter += 1
                klsInput .= (0, "")
        ]
        (do
          n$ input_
            [ type_ "text"
            , class_ "form-control ml-1"
            , value_ (st^.klsInput^._2)
            , oninput_ $ \inp _ -> do
                txt <- DOM.Input.getValue inp
                dispatch (klsInput._2 .= txt)
            ] ()
          n$ input_
            [ type_ "number"
            , class_ "form-control ml-1 mr-1"
            , value_ (tshow (st^.klsInput^._1))
            , oninput_ $ \inp _ -> do
                mbPrio <- readMaybe <$> DOM.Input.getValue inp
                F.for_ mbPrio (\prio -> dispatch (klsInput._1 .= prio))
            ] ()
          n$ button_
            [ type_ "submit"
            , class_ "btn btn-primary ml-1"
            ]
            (n$ "Add"))
    n$ div_ [class_ "row"] $ n$ div_ [class_ "col"] $ do
      F.for_ (sortBy (comparing (fst . snd)) (MS.toList (st^.klsElements))) $ \(k, (prio, txt)) -> do
        key (tshow k) $ div_ [class_ "row mt-1 mb-1"] $ do
            n$ div_ [class_ "col ml-1"] $ n$ text txt
            n$ div_ [class_ "col ml-1 mr-1"] $ n$ zoomT prio (klsElements.ix k._1) $ input_
              [ type_ "number"
              , class_ "form-control"
              , value_ (tshow prio)
              , onchange_ $ \inp _ -> do
                  mbPrio <- readMaybe <$> DOM.Input.getValue inp
                  F.for_ mbPrio (\prio' -> dispatch (put prio'))
              ] ()
            n$ div_ [class_ "col ml-1"] $ n$ a_
              [ href_ "#"
              , class_ "btn btn-primary"
              , onclick_ $ \_ ev -> do
                  preventDefault ev
                  dispatch (klsElements %= MS.delete k)
              ]
              (n$ text "Delete")

