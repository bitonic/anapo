module Main where

import Anapo.ClientM
import Anapo.Loop
import Anapo.Render
import Anapo.TestApps
import Anapo.TestApps.YouTube

main :: IO ()
main = runClientM $ do
  youTubeSetup
  st <- testAppsInit
  installComponentBootstrap RenderOptions{roAlwaysRerender = False, roDebugOutput = True} st testAppsComponent
