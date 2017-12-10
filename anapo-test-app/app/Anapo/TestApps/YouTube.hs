{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.YouTube
  ( YouTubeState
  , youTubeComponent
  , youTubeInit
  , youTubeSetup
  ) where


import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Foldable as F
import Data.Monoid ((<>))

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Document as DOM.Document
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Node as DOM.Node

import Anapo
import Anapo.Logging
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

-- it's important to have this as a generic element since we'll replace
-- it with a iframe
youTubeNode :: Node' DOM.Element YouTubeState
youTubeNode = do
  st <- askState
  mbYtpRef :: IORef (Maybe YouTubePlayer) <- liftIO (newIORef Nothing)
  let
    didMount el = void $ forkAction $ liftJSM $ do
      logInfo "Creating new YouTube object"
      -- We create the you tube element in the div _inside_ the
      -- top-level element otherwise anapo will choke on the fact that
      -- the element we inserted is gone.
      child <- DOM.Node.getFirstChildUnsafe =<< DOM.Node.getFirstChildUnsafe el
      ytp <- youTubeNew child YouTubeNew
        { ytnHeight = 390
        , ytnWidth = 640
        , ytnVideoId = st^.ytsVideoId
        }
      logInfo "Created new YouTube object"
      F.for_ (st^.ytsLastPosition) $ \t -> do
        logInfo ("Seeking video at " <> tshow t)
        youTubeSeekTo ytp t
        youTubePauseVideo ytp
      liftIO (writeIORef mbYtpRef (Just ytp))
  let
    willRemove _el = do
      mbYtp <- liftIO (readIORef mbYtpRef)
      case mbYtp of
        Nothing -> do
          logInfo "Note getting latest time -- no YT object"
          return ()
        Just ytp -> do
          logInfo "Getting latest time"
          t <- liftJSM (youTubeGetCurrentTime ytp)
          dispatch (ytsLastPosition .= Just t)
  -- we insert the to-be-replaced node as a raw node since otherwise the
  -- patching algorithm might try to patch it and fail because the YT
  -- library turns whatever we have into an iframe.
  el <- liftJSM $ do
    doc <- DOM.currentDocumentUnchecked
    container <- DOM.unsafeCastTo DOM.Element =<< DOM.Document.createElement doc ("div" :: Text)
    simpleRenderComponent container () $
      n$ div_ [class_ "row"] $ n$ div_ [class_ "col"] $
        n$ text "YouTube player not ready"
    return container
  rawNode DOM.Element el
    [ unsafeDidMount didMount
    , unsafeWillRemove willRemove
    ]

-- | Never rerender the node
youTubeComponent :: Component' YouTubeState
youTubeComponent = do
  -- TODO this causes a linking error with GHC! see TODO on 'marked'
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
