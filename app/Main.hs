module Main where

import Anapo.Core
import Anapo.Loop
import Anapo.Render
import Anapo.TestApps

main :: IO ()
main = runClientM $ do
  st <- testAppsInit
  installComponent RenderOptions{roAlwaysRerender = False, roDebugOutput = True} st testAppsComponent
