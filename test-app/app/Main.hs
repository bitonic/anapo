{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Anapo
import Anapo.Loop
import Anapo.Render
import Anapo.TestApps.Prelude

main :: IO ()
main = do
  installComponentBootstrap
    RenderOptions{roAlwaysRerender = False, roErase = True}
    dispatch getStateUpdate
    True (n$ marked (\_ _ -> Rerender) (static listComponent))
