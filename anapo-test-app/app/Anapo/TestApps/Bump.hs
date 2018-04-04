{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anapo.TestApps.Bump (BumpsState, bumpsNode, bumpsInit) where

import Anapo
import Anapo.TestApps.Prelude

bumpNode :: Text -> Node a Int
bumpNode title = do
  count <- ask
  div_ [] $ do
    n$ text (title <> ": ")
    n$ button_
      [ onclick_ $ \_ _ -> dispatch $
          modify (+ 1)
      ]
      (n$ "Bump")
    n$ text (" " <> tshow count)

data BumpsCycle =
    BCOne
  | BCTwo
  | BCThree

data BumpsState = BumpsState
  { _bumps1 :: Component Text () Int
  , _bumps2 :: Component Text () Int
  , _bumpsCycle :: BumpsCycle
  }
makeLenses ''BumpsState

bumpsNode :: Node () BumpsState
bumpsNode =
  div_ [] $ do
    n$ div_ [class_ "row"] $ n$ div_ [class_ "col"] $ do
      bs <- view bumpsCycle
      case bs of
        BCOne -> do
          n$ componentL bumps1 "1"
          n$ componentL bumps1 "1"
          n$ componentL bumps2 "2"
        BCTwo -> do
          n$ componentL bumps1 "1"
          n$ text "Blah"
          n$ componentL bumps2 "2"
        BCThree -> do
          n$ componentL bumps1 "1"
          n$ componentL bumps2 "2"
          n$ componentL bumps2 "2"
    n$ div_ [class_ "row mt-2"] $ n$ div_ [class_ "col"] $ do
      n$ button_
        [ onclick_ $ \_ _ -> dispatch $ do
            bumpsCycle %= \case
              BCOne -> BCTwo
              BCTwo -> BCThree
              BCThree -> BCOne
        ]
        (n$ "Swap")

bumpsInit :: Action () BumpsState BumpsState
bumpsInit = BumpsState
  <$> newComponent_ 0 bumpNode
  <*> newComponent_ 0 bumpNode
  <*> pure BCOne
