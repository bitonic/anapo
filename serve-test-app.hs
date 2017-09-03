#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import System.Environment (getArgs)
import System.FilePath
import qualified Data.Text as T

main :: IO ()
main = do
  [baseDir, htmlIndex] <- getArgs
  run 8000 $ \req respond -> do
    let fp = case pathInfo req of
          [jsFile] | ".js" `T.isSuffixOf` jsFile -> baseDir </> T.unpack jsFile
          _ -> htmlIndex
    respond (responseFile status200 [(hContentType, "text-html")] fp Nothing)
