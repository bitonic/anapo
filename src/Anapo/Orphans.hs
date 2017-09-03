module Anapo.Orphans () where

import Data.JSString (JSString)
import Data.Hashable (Hashable(..))
import System.IO.Unsafe (unsafePerformIO)

foreign import javascript unsafe
  " var hash = $1;\
  \ var str = $2;\
  \ var len = str.length;\
  \ if (len > 0) {\
  \   for (var i = 0; i < len; i++) {\
  \     hash = h$mulInt32(hash, 16777619) ^ str.charCodeAt(i);\
  \   }\
  \ }\
  \ $r = hash;"
  js_stringFnvHash :: Int -> JSString -> IO Int

-- see <https://github.com/ghcjs/shims/blob/b97015229c58eeab7c1d0bb575794b14a9f6efca/pkg/hashable.js#L10>
instance Hashable JSString where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt str = unsafePerformIO (js_stringFnvHash salt str)
