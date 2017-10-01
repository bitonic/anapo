module Main where

import Anapo.Loop
import Anapo.Render
import Anapo.TestApps
import Anapo.TestApps.YouTube

main :: IO ()
main = do
  youTubeSetup
  (dispatch, getStateUpdate) <- withDispatch
  testAppsWith dispatch $ \st -> do
    installComponentBootstrap
      RenderOptions{roAlwaysRerender = False, roDebugOutput = True}
      dispatch getStateUpdate
      st testAppsComponent
