{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.YouTubeBindings where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.Marshal
import qualified Data.Aeson.TH as Aeson
import Control.Exception.Safe (bracket)
import qualified Language.Javascript.JSaddle as JS

import Anapo.TestApps.Prelude

-- YT api bindings
-- --------------------------------------------------------------------

{-# NOINLINE youTubePlayerReady #-}
youTubePlayerReady :: MVar ()
youTubePlayerReady = unsafePerformIO newEmptyMVar

youTubeEnsureReady :: JSM ()
youTubeEnsureReady = liftIO (readMVar youTubePlayerReady)

-- blocks until yt api is ready
youTubeSetup :: JSM ()
youTubeSetup = do
  bracket
    (JS.asyncFunction $ \_ _ _ ->
      void (liftIO (tryPutMVar youTubePlayerReady ())))
    JS.freeFunction
    (\cback -> do
      (JS.jsg "window" JS.<# "onYouTubeIframeAPIReady") cback
      youTubeEnsureReady)

type YouTubePlayer = JSVal

data YouTubeNew = YouTubeNew
  { ytnHeight :: Int
  , ytnWidth :: Int
  , ytnVideoId :: Text
  }
Aeson.deriveJSON (aesonRecord "ytn") ''YouTubeNew

-- | blocks until the player is ready
youTubeNew ::
     (ToJSVal el)
  => el
  -> YouTubeNew
  -> JSM YouTubePlayer
youTubeNew container ytn = do
  -- make sure we have the api
  youTubeEnsureReady
  -- set up things
  containerJS <- JS.toJSVal container
  ytnJS <- JS.toJSVal_aeson ytn
  isPlayerReady <- liftIO newEmptyMVar
  bracket
    (JS.asyncFunction $ \_ _ _ ->
      void (liftIO (tryPutMVar isPlayerReady ())))
    JS.freeFunction
    (\cback -> do
      (ytnJS JS.<# "events") =<< JS.obj
      (ytnJS ^. JS.js "events" JS.<# "onReady") cback
      ytp <- JS.new (JS.jsg "YT" ^. JS.js "Player") [containerJS, ytnJS]
      liftIO (readMVar isPlayerReady)
      return ytp)

youTubeGetCurrentTime :: YouTubePlayer -> JSM Int
youTubeGetCurrentTime ytp =
  JS.fromJSValUnchecked =<< (ytp JS.# "getCurrentTime") ([] :: [JSVal])

youTubeSeekTo :: YouTubePlayer -> Int -> JSM ()
youTubeSeekTo ytp t =
  void ((ytp JS.# "seekTo") [t])

youTubePauseVideo :: YouTubePlayer -> JSM ()
youTubePauseVideo ytp = void ((ytp JS.# "pauseVideo") ([] :: [JSVal]))


