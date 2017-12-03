{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Anapo
import Anapo.Render
import Anapo.TestApps
import Anapo.TestApps.Prelude

#if defined(ghcjs_HOST_OS)
run :: JSM () -> IO ()
run = id
#else
import qualified Language.Javascript.JSaddle.Warp as Warp
run :: JSM () -> IO ()
run = Warp.run 8000
#endif

mainJSM :: JSM ()
mainJSM = do
  installComponentBootstrap
    RenderOptions
      { roAlwaysRerender = False
      , roErase = True
      , roSkipNode = \_ -> return False
      }
    (\cont -> testAppsWith (liftJSM . cont))
    testAppsComponent

main :: IO ()
main = run mainJSM
