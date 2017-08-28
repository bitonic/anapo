{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.YouTube (YouTubeState, youTubeComponent, youTubeInit) where

import Control.Lens (toListOf, _Just, makeLenses, (^.))

import qualified GHCJS.DOM.HTMLIFrameElement as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM as DOM
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Anapo
import Anapo.TestApps.Prelude

data YouTubeState = YouTubeState
  { _ytsNode :: DOM.Node
  , _ytsToken :: Int
  , _ytsCurrentVideo :: T.Text
  }
makeLenses ''YouTubeState

youTubeNode :: Node' DOM.Node YouTubeState
youTubeNode = do
  st <- askState
  rawNode (st^.ytsNode)

-- | Never rerender the node
youTubeComponent :: Component' YouTubeState
youTubeComponent = do
  dispatchM <- askDispatchM
  st <- askState
  bootstrapRow $ bootstrapCol $ do
    n$ marked
      (\trav prevSt st -> if toListOf (_Just . trav . ytsToken) prevSt == [st^.ytsToken]
          then UnsafeDontRerender
          else Rerender)
      (static youTubeNode)
  bootstrapRow $ bootstrapCol $ zoom' ytsCurrentVideo $ simpleTextInput
    (dispatchM (\st' -> youTubeInit (st'^.ytsCurrentVideo)))
    "Choose video"

{-# NOINLINE youTubeCounter #-}
youTubeCounter :: IORef Int
youTubeCounter = unsafePerformIO (newIORef 0)

youTubeInit :: T.Text -> ClientM YouTubeState
youTubeInit txt = do
  doc <- DOM.currentDocumentUnchecked
  iframe <- DOM.unsafeCastTo DOM.HTMLIFrameElement =<< DOM.createElement doc ("iframe" :: T.Text)
  DOM.setWidth iframe ("560" :: T.Text)
  DOM.setHeight iframe ("315" :: T.Text)
  DOM.setSrc iframe ("https://www.youtube.com/embed/" <> txt)
  DOM.setFrameBorder iframe ("0" :: T.Text)
  DOM.setAllowFullscreen iframe True
  iframeNode <- DOM.unsafeCastTo DOM.Node iframe
  counter <- liftIO (atomicModifyIORef' youTubeCounter (\c -> (c+1, c)))
  return (YouTubeState iframeNode counter txt)
