{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiWayIf #-}
module Anapo.TestApps.YouTube where -- (YouTubeState, youTubeComponent, youTubeInit, youTubeSetup) where

import Control.Lens (toListOf, makeLenses, (^.))

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Control.Concurrent.MVar
import GHCJS.Foreign.Callback
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Nullable
import qualified Data.Aeson.TH as Aeson
import Data.Char (toLower)
import Control.Monad (forM_)

import qualified GHCJS.DOM.HTMLIFrameElement as DOM
import qualified GHCJS.DOM.Types as DOM

import Anapo
import Anapo.TestApps.Prelude

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified Anapo.VDOM as V

-- YT api initialization
-- --------------------------------------------------------------------

{-# NOINLINE youTubePlayerReady #-}
youTubePlayerReady :: MVar ()
youTubePlayerReady = unsafePerformIO newEmptyMVar

foreign import javascript unsafe
  "window.onYouTubeIframeAPIReady = $1"
  js_setupOnYouTubeIframeAPIReady :: Callback (IO ()) -> IO ()

youTubeSetup :: ClientM ()
youTubeSetup = do
  cback <- asyncCallback $ do
    void (tryPutMVar youTubePlayerReady ())
  -- wait for thing to be ready and release callback
  void $ forkIO $ do
    readMVar youTubePlayerReady
    releaseCallback cback
  js_setupOnYouTubeIframeAPIReady cback

type YouTubePlayer = JSVal

data YouTubeNew = YouTubeNew
  { ytnHeight :: Int
  , ytnWidth :: Int
  , ytnVideoId :: T.Text
  }
Aeson.deriveJSON
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \s -> case s of
        'y':'t':'n':c:rest -> toLower c : rest
        _ -> error "Bad YouTubeNew prefix!"
    }
  ''YouTubeNew

foreign import javascript unsafe
  "new YT.Player($1, $2)"
  js_youTubeNew :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
  " $1.events = {};\
  \ $1.events.onReady = function(event) {\
  \   console.log(\"player ready\");\
  \   if ($2) {\
  \     event.target.seekTo($2, true);\
  \   }\
  \   if ($3) {\
  \     event.target.playVideo();\
  \   }\
  \ };"
  js_youTubePlayAndSeekOnReady :: JSVal -> Nullable Int -> Bool -> IO ()

-- | this will block waiting for youtube api to be ready
youTubeNew ::
     (ToJSVal el)
  => el
  -> YouTubeNew
  -> Maybe Int -- ^ seek when ready
  -> Bool -- ^ play when ready
  -> IO YouTubePlayer
youTubeNew container ytn mbSeek playOnReady = do
  readMVar youTubePlayerReady
  containerJS <- toJSVal container
  ytnJS <- toJSVal_aeson ytn
  js_youTubePlayAndSeekOnReady ytnJS (maybeToNullable mbSeek) playOnReady
  ytp <- js_youTubeNew containerJS ytnJS
  return ytp

foreign import javascript unsafe
  "$1.getCurrentTime()"
  js_youTubeGetCurrentTime :: JSVal -> IO Int

youTubeGetCurrentTime :: YouTubePlayer -> IO Int
youTubeGetCurrentTime = js_youTubeGetCurrentTime

foreign import javascript unsafe
  "$1.getPlayerState()"
  js_youTubeGetPlayerState :: JSVal -> IO Int

data YouTubePlayerState =
    YTPSUnstarted
  | YTPSEnded
  | YTPSPlaying
  | YTPSPaused
  | YTPSBuffering
  | YTPSVideoCued

youTubeGetPlayerState :: YouTubePlayer -> IO YouTubePlayerState
youTubeGetPlayerState ytp = do
  i <- js_youTubeGetPlayerState ytp
  if
    | i == -1 -> return YTPSUnstarted
    | i == 0 -> return YTPSEnded
    | i == 1 -> return YTPSPlaying
    | i == 2 -> return YTPSPaused
    | i == 3 -> return YTPSBuffering
    | i == 5 -> return YTPSVideoCued
    | True -> fail ("Invalid player state " ++ show i)

foreign import javascript unsafe
  "$1.destroy()"
  js_youTubeDestroy :: JSVal -> IO ()

youTubeDestroy :: YouTubePlayer -> IO ()
youTubeDestroy = js_youTubeDestroy

-- YT api
-- --------------------------------------------------------------------

data YouTubeStatus =
    YTSMounted YouTubePlayer
  | YTSUnmounted
      -- nothing if it has never been mounted
      (Maybe
        ( Int -- ^ number of seconds passed
        , YouTubePlayerState -- ^ player state
        ))

data YouTubeState = YouTubeState
  { _ytsToken :: Int
  , _ytsVideoId :: T.Text
  , _ytsStatus :: IORef YouTubeStatus
  }
makeLenses ''YouTubeState

youTubeNode :: Node' DOM.HTMLDivElement YouTubeState
youTubeNode = do
  st <- askState
  {-
  unsafeDidMount (didMount st) $
    unsafeWillRemove (\_el -> willRemove st) $
      div_
  -}
  nod <- div_
  return nod
    { V.nodeCallbacks = mempty
        { V.callbacksUnsafeDidMount = didMount st
        , V.callbacksUnsafeWillRemove = willRemove st
        }
    }
  where
    didMount yts el = do
      -- get current state
      st <- readIORef (yts^.ytsStatus)
      let (mbSeek, playOnReady) = case st of
            YTSUnmounted (Just (secs, ps)) ->
              ( Just secs
              , case ps of
                  YTPSUnstarted -> False
                  YTPSEnded -> False
                  YTPSPlaying -> True
                  YTPSPaused -> False
                  YTPSBuffering -> True
                  YTPSVideoCued -> False
              )
            YTSUnmounted Nothing -> (Nothing, False)
            YTSMounted{} -> (Nothing, False)
      -- Just cancel any eventual dangling stuff...
      willRemove yts el
      -- insert container div and replace it with player
      doc <- DOM.currentDocumentUnchecked
      toReplace <- DOM.createElement doc ("div" :: T.Text)
      DOM.appendChild_ el toReplace
      ytp <- youTubeNew
        toReplace
        YouTubeNew
          { ytnWidth = 640
          , ytnHeight = 390
          , ytnVideoId = (yts^.ytsVideoId)
          }
        mbSeek playOnReady
      writeIORef (yts^.ytsStatus) (YTSMounted ytp)

    removeAllChildren :: (DOM.IsNode el) => el -> ClientM ()
    removeAllChildren el = let
      go = do
        mbChild <- DOM.getFirstChild el
        forM_ mbChild $ \child -> do
          DOM.removeChild_ el child
          go
      in go

    willRemove yts el = do
      st <- readIORef (yts^.ytsStatus)
      case st of
        YTSMounted ytp -> do
          t <- youTubeGetCurrentTime ytp
          st <- youTubeGetPlayerState ytp
          youTubeDestroy ytp
          writeIORef (yts^.ytsStatus) (YTSUnmounted (Just (t, st)))
        YTSUnmounted{} -> return ()
      -- destroy seems to swap the old element back,
      -- wipe it
      removeAllChildren el

-- | Never rerender the node
youTubeComponent :: Component' YouTubeState
youTubeComponent = do
  dispatchM <- askDispatchM
  bootstrapRow $ bootstrapCol $ do
    n$ marked
      (\trav prevSt st -> case toMaybeOf (trav . ytsToken) prevSt of
        Just tok -> if tok == st^.ytsToken
          then UnsafeDontRerender
          else Rerender
        Nothing -> Rerender)
      (static youTubeNode)
  bootstrapRow $ bootstrapCol $ zoom' ytsVideoId $ simpleTextInput
    (dispatchM (\st' -> youTubeInit (st'^.ytsVideoId)))
    "Choose video"

{-# NOINLINE youTubeCounter #-}
youTubeCounter :: IORef Int
youTubeCounter = unsafePerformIO (newIORef 0)

youTubeInit :: T.Text -> ClientM YouTubeState
youTubeInit txt = do
  counter <- liftIO (atomicModifyIORef' youTubeCounter (\c -> (c+1, c)))
  ref <- newIORef (YTSUnmounted Nothing)
  return (YouTubeState counter txt ref)
