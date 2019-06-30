{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Anapo
import Anapo.TestApps
import Anapo.TestApps.YouTube
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
  youTubeSetup
  installNodeBody
    (\cont -> testAppsWith (liftJSM . cont))
    testAppsComponent
    (const True)
    (const True)
    (\exc -> testAppsError (tshow exc))
    IMAppend

main :: IO ()
main = run mainJSM
