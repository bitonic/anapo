{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.ComponentDifferentNodes
  ( componentDifferentNodesInit
  ) where

import Anapo
import Anapo.TestApps.Prelude

componentDifferentNodesInit ::
     MonadJSM m
  => m (Component () () Bool)
componentDifferentNodesInit = do
  newComponent True $ \_ -> do
    b <- view state
    let toggle = do
          n$ a_
            [ href_ "#"
            , onclick_ $ \_ ev -> do
                preventDefault ev
                dispatch (modify not)
            ]
            (n$ "Toggle")
    if b
      then p_ [] $ do
        toggle
        n$ "This is a p"
      else div_ [] $ do
        toggle
        n$ "This is a div"
