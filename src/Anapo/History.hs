module Anapo.History where

import Data.JSString (JSString)
import GHCJS.Types (JSVal)
import GHCJS.Foreign.Callback (Callback)

foreign import javascript unsafe
  "history.pushState($1, $2, $3)"
  historyPushState :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe
  "history.replaceState($1, $2, $3)"
  historyReplaceState :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe
  "window.onpopstate = function (event) { $1(event.state); };"
  historySetOnPopState :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "window.onpopstate = null;"
  historyRemoveOnPopState :: IO ()

