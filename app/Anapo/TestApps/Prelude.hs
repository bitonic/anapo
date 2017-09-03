{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Anapo.TestApps.Prelude
  ( module X
  , module Anapo.TestApps.Prelude
  ) where

import Control.Monad.IO.Class as X (liftIO)
import Control.Monad as X (void)
import Control.Concurrent as X (forkIO)
import qualified Data.Aeson.TH as Aeson
import Data.List (isPrefixOf)
import Data.Char (toLower)
import qualified Data.JSString as JSS
import Data.JSString (JSString)

import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM

import Anapo
import Anapo.Utils

#if !defined(ghcjs_HOST_OS)
import Language.Javascript.JSaddle.Monad (JSM(..))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader (ask, runReaderT)

deriving instance MonadBase IO JSM

instance MonadBaseControl IO JSM where
  type StM JSM a = a
  {-# INLINE liftBaseWith #-}
  liftBaseWith cont = do
    x <- JSM ask
    liftIO (cont (\(JSM m) -> runReaderT m x))
  {-# INLINE restoreM #-}
  restoreM = return
#endif

jsshow :: (Show a) => a -> JSString
jsshow = JSS.pack . show

bootstrapRow :: Component read write -> Component read write
bootstrapRow = n . div_ (class_ "row")

bootstrapCol :: Component read write -> Component read write
bootstrapCol = n . div_ (class_ "col")

booleanCheckbox :: Node' DOM.HTMLInputElement Bool
booleanCheckbox = do
  st <- askState
  dispatch <- askDispatch
  input_
    (class_ "form-check-input mr-1")
    (type_ "checkbox")
    (checked_ st)
    (onchange_ $ \el ev -> do
      DOM.preventDefault ev
      checked <- DOM.getChecked el
      dispatch (const checked))

simpleTextInput ::
     JSString
  -- ^ the label for the input
  -> ClientM ()
  -- ^ what to do when the new text is submitted
  -> JSString
  -- ^ what to show in the button
  -> Component' JSString
simpleTextInput lbl cback buttonTxt = do
  currentTxt <- askState
  dispatch <- askDispatch
  n$ form_
    (class_ "form-inline mx-1 my-2")
    (onsubmit_ $ \_ ev -> do
      DOM.preventDefault ev
      cback)
    (do
      n$ input_
        (type_ "text")
        (class_ "form-control mb-2 mr-sm-2 mb-sm-0")
        (value_ currentTxt)
        (rawProperty_ "aria-label" (jsStringToJSVal lbl))
        (oninput_ $ \inp _ -> do
          txt <- DOM.getValue inp
          dispatch (const txt))
      n$ button_
        (type_ "submit")
        (class_ "btn btn-primary")
        (n$ text buttonTxt))

aesonRecord :: String -> Aeson.Options
aesonRecord prefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = \s -> if prefix `isPrefixOf` s && length s > length prefix
      then let
        ~(c : rest) = drop (length prefix) s
        in toLower c : rest
      else error ("record label " ++ s ++ " does not have prefix " ++ prefix)
  }
