{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.SlowRequest where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson as Aeson
import Control.Exception.Safe (try)
import Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified GHCJS.DOM.XMLHttpRequest as DOM.XHR

import Anapo
import Anapo.CollectedThread
import Anapo.TestApps.Prelude

type Posts = V.Vector Post

data Post = Post
  { postUserId :: Int
  , postId :: Int
  , postTitle :: Text
  , postBody :: Text
  } deriving (Eq, Show)
Aeson.deriveJSON (aesonRecord "post") ''Post

data SlowRequestState =
    SRSNotLoaded
  | SRSLoading CollectedThreadId
  | SRSError Text
  | SRSLoaded Posts

slowRequestInit :: JSM SlowRequestState
slowRequestInit = return SRSNotLoaded

slowRequestComponent :: Component' SlowRequestState
slowRequestComponent = do
  st <- askState
  let
    startLoading = do
      tid <- forkCollected $ do
        xhr <- DOM.XHR.newXMLHttpRequest
        error "TODO"
      put (SRSLoading tid)
    {-
    startLoading = do
      reqAsync <- async $ do
        mbResp :: Either XHR.XHRError (XHR.Response ByteString) <- try $ XHR.xhrByteString XHR.Request
          { XHR.reqMethod = XHR.GET
          , XHR.reqURI = "http://slowwly.robertomurray.co.uk/delay/5000/url/https://jsonplaceholder.typicode.com/posts"
          , XHR.reqLogin = Nothing
          , XHR.reqHeaders = []
          , XHR.reqWithCredentials = False
          , XHR.reqData = XHR.NoData
          }
        dispatch $ \_ -> return $ case mbResp of
          Left err -> SRSError (tshow err)
          Right resp -> do
            if XHR.status resp /= 200
              then SRSError ("Bad status " <> tshow (XHR.status resp))
              else case XHR.contents resp of
                Nothing -> SRSError "No contents"
                Just bytes -> case Aeson.eitherDecode (BSL.fromStrict bytes) of
                  Left err -> SRSError ("Could not decode: " <> tshow err)
                  Right posts -> SRSLoaded posts
      return (SRSLoading reqAsync)
    -}
  n$ button_
    (type_ "button")
    (class_ "btn btn-primary")
    (disabled_ $ case st of
      SRSNotLoaded{} -> False
      SRSLoading{} -> True
      SRSLoaded{} -> False
      SRSError{} -> False)
    (onclick_ $ \_ ev -> do
      preventDefault ev
      dispatch $ get >>= \case
        SRSLoading{} -> return ()
        SRSError{} -> startLoading
        SRSNotLoaded{} -> startLoading
        SRSLoaded{} -> startLoading)
      (n$ case st of
        SRSNotLoaded -> "Load posts"
        SRSLoading{} -> "Loading"
        SRSError{} -> "Try again"
        SRSLoaded{} -> "Refresh")
  case st of
    SRSNotLoaded -> return ()
    SRSLoading{} -> return ()
    SRSError err -> n$ div_ (class_ "alert alert-danger") (n$ text err)
    SRSLoaded posts ->
      n$ ul_ (class_ "list-group mt-2") $ F.for_ posts $ \Post{..} ->
        key (tshow postId) $ li_
          (class_ "list-group-item list-group-item-action flex-column align-items-start")
          (do
            n$ h5_ (class_ "mb-1") (n$ text postTitle)
            n$ p_ (class_ "mb-1") (n$ text postBody))
