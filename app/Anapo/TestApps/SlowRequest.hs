{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.SlowRequest where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson as Aeson
import qualified JavaScript.Web.XMLHttpRequest as XHR
import Control.Concurrent.Async (Async, async)
import Control.Exception.Safe (try)
import Data.Monoid ((<>))
import Data.Foldable (for_)
import Data.JSString (JSString)
import Data.JSString.Text (textToJSString)
import Data.Text (Text)

import Anapo
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
  | SRSLoading (Async ())
  | SRSError JSString
  | SRSLoaded Posts

slowRequestInit :: ClientM SlowRequestState
slowRequestInit = return SRSNotLoaded

slowRequestComponent :: Component' SlowRequestState
slowRequestComponent = do
  st <- askState
  dispatchM <- askDispatchM
  dispatch <- askDispatch
  let
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
        dispatch $ \_ -> case mbResp of
          Left err -> SRSError (jsshow err)
          Right resp -> do
            if XHR.status resp /= 200
              then SRSError ("Bad status " <> jsshow (XHR.status resp))
              else case XHR.contents resp of
                Nothing -> SRSError "No contents"
                Just bytes -> case Aeson.eitherDecode (BSL.fromStrict bytes) of
                  Left err -> SRSError ("Could not decode: " <> jsshow err)
                  Right posts -> SRSLoaded posts
      return (SRSLoading reqAsync)
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
      dispatchM $ \case
        SRSLoading asy -> return (SRSLoading asy)
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
      n$ ul_ (class_ "list-group mt-2") $ for_ posts $ \Post{..} ->
        key (jsshow postId) $ li_
          (class_ "list-group-item list-group-item-action flex-column align-items-start")
          (do
            n$ h5_ (class_ "mb-1") (n$ text (textToJSString postTitle))
            n$ p_ (class_ "mb-1") (n$ text (textToJSString postBody)))
