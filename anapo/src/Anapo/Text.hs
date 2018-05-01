{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)

{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Anapo.Text (Text, module Data.JSString) where

import Data.JSString
import Data.JSString.Text
import Data.Hashable (Hashable(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Aeson as Aeson

foreign import javascript unsafe "$r = h$stringFnvHash($1, $2)"
  js_stringFnvHash :: Int -> JSString -> IO Int

-- see <https://github.com/ghcjs/shims/blob/b97015229c58eeab7c1d0bb575794b14a9f6efca/pkg/hashable.js#L10>
instance Hashable JSString where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt str = unsafePerformIO (js_stringFnvHash salt str)

instance Aeson.ToJSON JSString where
  {-# INLINE toJSON #-}
  toJSON = Aeson.String . textFromJSString

instance Aeson.FromJSON JSString where
  {-# INLINE parseJSON #-}
  parseJSON = Aeson.withText "JSString" (return . textToJSString)

type Text = JSString

#else

module Anapo.Text (module Data.Text) where

import Data.Text

#endif
