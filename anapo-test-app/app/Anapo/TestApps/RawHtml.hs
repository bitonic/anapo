{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.RawHtml (rawHtmlComponent) where

import Control.Monad (when)

import Anapo
import Anapo.TestApps.Prelude

rawHtmlComponent :: Node a Bool
rawHtmlComponent = div_ [] $ do
  b <- view state
  n$ a_
    [ href_ "#"
    , onclick_ $ \_ ev -> do
        preventDefault ev
        dispatch (modify not)
    ] (n$ "TOGGLE")
  when b (n$ div_ [class_ "m-2"] (n$ "Hello"))
  n$ div_ [class_ "m-2"] (UnsafeRawHtml "WORLD")

