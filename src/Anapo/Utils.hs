module Anapo.Utils where

import Data.JSString (JSString)
import GHCJS.Types (JSVal)
import Unsafe.Coerce (unsafeCoerce)

jsStringToJSVal :: JSString -> JSVal
jsStringToJSVal = unsafeCoerce
