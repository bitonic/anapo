module Anapo.TestApps.HogJowls (HogJowlsState, hogJowlsComponent, hogJowlsInit) where

import Control.Lens (toListOf, _Just, makeLenses, (^.))

import qualified GHCJS.DOM.HTMLIFrameElement as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM as DOM
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Anapo
import Anapo.TestApps.Prelude

data HogJowlsState = HogJowlsState
  { _hjsNode :: DOM.Node
  , _hjsToken :: Int
  }
makeLenses ''HogJowlsState

hogJowlsNode :: Node' DOM.Node HogJowlsState
hogJowlsNode = do
  st <- askState
  rawNode (st^.hjsNode)

-- | Never rerender the node
hogJowlsComponent :: Component' HogJowlsState
hogJowlsComponent =
  n$ marked
    (\trav prevSt st -> if toListOf (_Just . trav . hjsToken) prevSt == [st^.hjsToken]
        then UnsafeDontRerender
        else Rerender)
    (static hogJowlsNode)

{-# NOINLINE hogJowsCounter #-}
hogJowsCounter :: IORef Int
hogJowsCounter = unsafePerformIO (newIORef 0)

hogJowlsInit :: ClientM HogJowlsState
hogJowlsInit = do
  doc <- DOM.currentDocumentUnchecked
  iframe <- DOM.unsafeCastTo DOM.HTMLIFrameElement =<< DOM.createElement doc "iframe"
  DOM.setWidth iframe "560"
  DOM.setHeight iframe "315"
  DOM.setSrc iframe "https://www.youtube.com/embed/Hah4iGqh7GY"
  DOM.setFrameBorder iframe "0"
  DOM.setAllowFullscreen iframe True
  iframeNode <- DOM.unsafeCastTo DOM.Node iframe
  counter <- liftIO (atomicModifyIORef' hogJowsCounter (\c -> (c, c+1)))
  return (HogJowlsState iframeNode counter)
