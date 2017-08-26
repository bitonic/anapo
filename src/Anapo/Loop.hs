module Anapo.Loop where

import Control.Exception (BlockedIndefinitelyOnMVar, try)
import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM

import Anapo.Core
import Anapo.Component
import Anapo.Render

componentLoop :: forall state acc.
     (Show state)
  => state -> Component state
  -> acc -> (acc -> VirtualDom -> ClientM acc)
  -> ClientM acc
  -- ^ returns the final accumulator, when there is nothing left to do.
  -- might never terminate
componentLoop !st0 vdom !acc0 useDom = do
  toDispatch :: Chan (state -> ClientM state) <- liftIO newChan
  let
    go :: Maybe state -> state -> acc -> ClientM acc
    go mbPrevSt !st !acc = do
      let nodes = runComponent vdom (liftIO . writeChan toDispatch) mbPrevSt st
      acc' <- useDom acc nodes
      mbF :: Either BlockedIndefinitelyOnMVar (state -> ClientM state) <- liftIO (try (readChan toDispatch))
      case mbF of
        Left{} -> do
          liftIO (putStrLn "BLOCKED FOREVER ON MVAR, TERMINATING COMPONENT LOOP")
          return acc'
        Right f -> do
          st' <- f st
          liftIO (putStrLn "NEW STATE")
          liftIO (putStrLn (show st'))
          go (Just st) st' acc'
  go Nothing st0 acc0

installComponent :: Show state => RenderOptions -> state -> Component state -> ClientM ()
installComponent ro st0 vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  void $ componentLoop st0 vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc body mbVdom vdom
    DOM.syncPoint -- force jssaddle stuff
    return (Just (vdom, evts))
