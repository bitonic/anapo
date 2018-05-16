{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Anapo.TestApps.Orphans () where

#if defined(ghcjs_HOST_OS)
import Data.JSString
import Data.JSString.Text
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON JSString where
  {-# INLINE toJSON #-}
  toJSON = Aeson.String . textFromJSString

instance Aeson.FromJSON JSString where
  {-# INLINE parseJSON #-}
  parseJSON = Aeson.withText "JSString" (return . textToJSString)

#endif
