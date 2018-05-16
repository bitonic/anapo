{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Anapo.TestApps.Prelude
  ( module X
  , module Anapo.TestApps.Prelude
  ) where

import Control.Monad.IO.Class as X (liftIO)
import Control.Monad as X (void)
import Control.Concurrent as X (killThread)
import Control.Lens as X (makePrisms, makeLenses, over, (^.), set, ix, (%=), use, (.=), at, Lens', view)
import GHCJS.DOM.Types as X (JSM, liftJSM, MonadJSM)
import Anapo.Text as X (Text)
import Control.Monad.State as X (put, get, modify)
import Control.Monad.Reader as X (ask)
import Control.Monad.IO.Unlift as X (unliftIO, askUnliftIO)
import Data.Monoid as X ((<>))

import qualified Data.Aeson.TH as Aeson
import Data.List (isPrefixOf)
import Data.Char (toLower)
import qualified GHCJS.DOM.HTMLInputElement as DOM
import Anapo
import qualified Anapo.Text as T
import Anapo.TestApps.Orphans ()

tshow :: (Show a) => a -> Text
tshow = T.pack . show

booleanCheckbox :: Node a Bool
booleanCheckbox = do
  st <- ask
  input_
    [ class_ "form-check-input mr-1"
    , type_ "checkbox"
    , checked_ st
    , onchange_ $ \el ev -> do
        preventDefault ev
        checked <- DOM.getChecked el
        dispatch (put checked)
    ]

data SimpleTextInputProps = STIP
  { stipButtonText :: Text
  , stipOnSubmit :: JSM ()
  }

simpleTextInput ::
     SimpleTextInputProps
  -> Node a Text
simpleTextInput STIP{..} = do
  currentTxt <- ask
  form_
    [ class_ "form-inline mx-1 my-2"
    , onsubmit_ $ \_ ev -> liftJSM $ do
        preventDefault ev
        stipOnSubmit
    ]
    (do
      n$ input_
        [ type_ "text"
        , class_ "form-control mb-2 mr-sm-2 mb-sm-0"
        , value_ currentTxt
        , onchange_ $ \inp _ -> do
            txt <- DOM.getValue inp
            dispatch (put txt)
        ]
      n$ button_
        [ type_ "submit"
        , class_ "btn btn-primary"
        ]
        (n$ text stipButtonText))

aesonRecord :: String -> Aeson.Options
aesonRecord prefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = \s -> if prefix `isPrefixOf` s && length s > length prefix
      then let
        ~(c : rest) = drop (length prefix) s
        in toLower c : rest
      else error ("record label " ++ s ++ " does not have prefix " ++ prefix)
  }
