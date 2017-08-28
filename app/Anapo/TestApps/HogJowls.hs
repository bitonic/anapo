module Anapo.TestApps.HogJowls (HogJowlsState, hogJowlsComponent, hogJowlsInit) where

import Control.Lens (toListOf, _Just, makeLenses, (^.))
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4

import qualified GHCJS.DOM.HTMLIFrameElement as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM as DOM

import Anapo
import Anapo.TestApps.Prelude

data HogJowlsState = HogJowlsState
  { _hjsNode :: DOM.Node
  , _hjsToken :: UUID
  }
makeLenses ''HogJowlsState

-- | Never rerender the node
hogJowlsComponent :: Component' HogJowlsState
hogJowlsComponent = do
  st <- askState
  rer <- askPreviousState $ \trav prevSt -> return$
    if toListOf (_Just . trav . hjsToken) prevSt == [st^.hjsToken]
      then UnsafeDontRerender
      else Rerender
  n$ rerender rer (rawNode (st^.hjsNode))

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
  HogJowlsState iframeNode <$> liftIO V4.nextRandom
