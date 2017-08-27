{-# LANGUAGE OverloadedStrings #-}
module Anapo.TestApps.Prelude
  ( module X
  , module Anapo.TestApps.Prelude
  ) where

import Control.Monad.IO.Class as X (liftIO)
import qualified Data.Text as T

import Anapo.Component

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

bootstrapRow :: Component read write -> Component read write
bootstrapRow = n . div_ (class_ "row")

bootstrapCol :: Component read write -> Component read write
bootstrapCol = n . div_ (class_ "col")
