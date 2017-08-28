module Anapo.TestApps.HogJowls (HogJowlsState, hogJowlsComponent, hogJowlsInit) where

import qualified GHCJS.DOM.HTMLIFrameElement as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM as DOM

import Anapo

newtype HogJowlsState = HogJowlsState DOM.HTMLIFrameElement

hogJowlsNode :: Node' HogJowlsState
hogJowlsNode = do
  HogJowlsState iframe <- askState
  rawNode DOM.HTMLIFrameElement iframe mempty

-- | Never rerender the node
hogJowlsComponent :: Component' HogJowlsState
hogJowlsComponent = n$ marked (\_ _ _ -> DontRerender) (static hogJowlsNode)

hogJowlsInit :: ClientM HogJowlsState
hogJowlsInit = do
  doc <- DOM.currentDocumentUnchecked
  iframe <- DOM.unsafeCastTo DOM.HTMLIFrameElement =<< DOM.createElement doc "iframe"
  DOM.setWidth iframe "560"
  DOM.setHeight iframe "315"
  DOM.setSrc iframe "https://www.youtube.com/embed/Hah4iGqh7GY"
  DOM.setFrameBorder iframe "0"
  DOM.setAllowFullscreen iframe True
  return (HogJowlsState iframe)
