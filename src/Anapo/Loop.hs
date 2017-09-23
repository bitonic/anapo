module Anapo.Loop where

import Control.Exception (BlockedIndefinitelyOnMVar, try)
import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM

import qualified Anapo.VDOM as V
import Anapo.Component
import Anapo.Render
import Anapo.ClientM

withDispatch :: ClientM (Dispatch state, ClientM (Maybe (state -> ClientM state)))
withDispatch = do
  toDispatch :: Chan (state -> ClientM state) <- liftIO newChan
  let
    get = do
      mbF :: Either BlockedIndefinitelyOnMVar (state -> ClientM state) <- liftIO (try (readChan toDispatch))
      case mbF of
        Left{} -> do
          liftIO (putStrLn "withDispatch: got undefinitedly blocked on mvar, returning Nothing")
          return Nothing
        Right f -> return (Just f)
  return (liftIO . writeChan toDispatch, get)

componentLoop :: forall state acc.
     Dispatch state
  -> ClientM (Maybe (state -> ClientM state))
  -> state
  -> Component' state
  -> acc -> (acc -> V.Dom -> ClientM acc)
  -> ClientM acc
  -- ^ returns the final accumulator, when there is nothing left to do.
  -- might never terminate
componentLoop dispatch getStateUpdate !st0 vdom !acc0 useDom = do
  let
    go :: Maybe state -> state -> acc -> ClientM acc
    go mbPrevSt !st !acc = do
      nodes <- runComponent vdom dispatch mbPrevSt st
      acc' <- useDom acc nodes
      mbF <- getStateUpdate
      case mbF of
        Nothing -> do
          liftIO (putStrLn "No state update received, terminating component loop")
          return acc'
        Just f -> do
          st' <- f st
          liftIO (putStrLn "State updated, wil re render")
          go (Just st) st' acc'
  go Nothing st0 acc0

installComponentBody ::
     RenderOptions
  -> Dispatch state
  -> ClientM (Maybe (state -> ClientM state))
  -> state
  -> Component' state
  -> ClientM ()
installComponentBody ro dispatch getStateUpdate st0 vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  void $ componentLoop dispatch getStateUpdate st0 vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc body mbVdom vdom
    DOM.syncPoint -- force jssaddle stuff
    return (Just (vdom, evts))

installComponentBootstrap ::
     RenderOptions
  -> Dispatch state
  -> ClientM (Maybe (state -> ClientM state))
  -> state
  -> Component' state
  -> ClientM ()
installComponentBootstrap ro dispatch getStateUpdate st0 vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  container <- DOM.unsafeCastTo DOM.HTMLDivElement =<< DOM.createElement doc "div"
  DOM.setClassName container "container"
  DOM.appendChild_ body container
  void $ componentLoop dispatch getStateUpdate st0 vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc container mbVdom vdom
    DOM.syncPoint -- force jssaddle stuff
    return (Just (vdom, evts))

