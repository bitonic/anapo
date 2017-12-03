{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.YouTube
  ( YouTubeState
  , youTubeComponent
  , youTubeInit
  , youTubeSetup
  ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Foldable as F

import qualified GHCJS.DOM.Types as DOM

import Anapo
import Anapo.TestApps.Prelude
import Anapo.TestApps.YouTubeBindings


-- YT api
-- --------------------------------------------------------------------

type YouTubeLastPosition = Maybe Int -- ^ seconds elapsed

type VideoId = Text

data YouTubeState = YouTubeState
  { _ytsVideoId :: VideoId
  , _ytsToken :: Int
  , _ytsLastPosition :: YouTubeLastPosition
  }
makeLenses ''YouTubeState

{-# NOINLINE youTubeCounter #-}
youTubeCounter :: IORef Int
youTubeCounter = unsafePerformIO (newIORef 0)

newYouTubeToken :: JSM Int
newYouTubeToken = liftIO (atomicModifyIORef' youTubeCounter (\c -> (c+1, c)))

youTubeInit :: DOM.MonadJSM m => VideoId -> m YouTubeState
youTubeInit videoId = liftJSM $ do
  tok <- newYouTubeToken
  return YouTubeState
    { _ytsVideoId = videoId
    , _ytsToken = tok
    , _ytsLastPosition = Nothing
    }

-- it's important to have this as a generic element since we'll replace it
-- with a iframe
youTubeNode :: Node' DOM.HTMLDivElement YouTubeState
youTubeNode = do
  st <- askState
  mbYtpRef :: IORef (Maybe YouTubePlayer) <- liftIO (newIORef Nothing)
  let
    didMount el = void $ forkAction $ liftJSM $ do
      ytp <- youTubeNew el YouTubeNew
        { ytnHeight = 390
        , ytnWidth = 640
        , ytnVideoId = st^.ytsVideoId
        }
      F.for_ (st^.ytsLastPosition) $ \t -> do
        youTubeSeekTo ytp t
        youTubePauseVideo ytp
      liftIO (writeIORef mbYtpRef (Just ytp))
  let
    willRemove _el = do
      mbYtp <- liftIO (readIORef mbYtpRef)
      case mbYtp of
        Nothing -> return ()
        Just ytp -> do
          t <- liftJSM (youTubeGetCurrentTime ytp)
          dispatch (ytsLastPosition .= Just t)
  div_
    [ class_ "row"
    , unsafeDidMount didMount
    , unsafeWillRemove willRemove
    ]
    (n$ div_ [class_ "col"] $
      n$ "YouTube player not ready")

-- | Never rerender the node
youTubeComponent :: Component' YouTubeState
youTubeComponent = do
  -- TODO this causes a linking error with GHC! see TODO
  -- on 'marked'
  n$ marked
    (\mbPrevSt st -> case mbPrevSt of
      Just prevSt -> if prevSt^.ytsToken == st^.ytsToken
        then UnsafeDontRerender
        else Rerender
      Nothing -> Rerender)
    (static youTubeNode)
  n$ div_ [class_ "row"] $ n$ div_ [class_ "col"] $
    simpleTextInput "video id" ytsVideoId
      (dispatch $ do
        vid <- use ytsVideoId
        put =<< liftJSM (youTubeInit vid))
      "Choose video"
