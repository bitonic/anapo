{-# LANGUAGE CPP #-}
module Anapo.Debug (debuggingEnabled) where
  
#if defined(ghcjs_HOST_OS)

import GHCJS.Types (JSVal)
import qualified GHCJS.Foreign.Internal as JS

foreign import javascript unsafe
  "window.anapoDebug"
  js_isDebug :: IO JSVal

debuggingEnabled :: IO Bool
debuggingEnabled = do
  v <- js_isDebug
  return (JS.isTruthy v)

#else

debuggingEnabled :: IO Bool
debuggingEnabled = return True

#endif
