module Main where

import Anapo.Core
import Anapo.Loop
import Anapo.TestApps

main :: IO ()
main = runClientM $ do
  st <- testAppsInit
  installComponent st testAppsComponent
