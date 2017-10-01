module Anapo.Loop where

import Control.Exception (BlockedIndefinitelyOnMVar, try)
import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM

import qualified Anapo.VDOM as V
import Anapo.Component
import Anapo.Render

withDispatch :: DOM.JSM (Dispatch state, DOM.JSM (Maybe (state -> DOM.JSM state)))
withDispatch = do
  toDispatch :: Chan (state -> DOM.JSM state) <- liftIO newChan
  let
    get = do
      mbF :: Either BlockedIndefinitelyOnMVar (state -> DOM.JSM state) <- liftIO (try (readChan toDispatch))
      case mbF of
        Left{} -> do
          liftIO (putStrLn "withDispatch: got undefinitedly blocked on mvar, returning Nothing")
          return Nothing
        Right f -> return (Just f)
  return (liftIO . writeChan toDispatch, get)

timeIt :: DOM.JSM a -> DOM.JSM (a, NominalDiffTime)
timeIt m = do
  t0 <- liftIO getCurrentTime
  x <- m
  t1 <- liftIO getCurrentTime
  return (x, diffUTCTime t1 t0)

componentLoop :: forall state acc.
     RenderOptions
  -> Dispatch state
  -> DOM.JSM (Maybe (state -> DOM.JSM state))
  -> state
  -> Component' state
  -> acc -> (acc -> V.Dom -> DOM.JSM acc)
  -> DOM.JSM acc
  -- ^ returns the final accumulator, when there is nothing left to do.
  -- might never terminate
componentLoop ro dispatch getStateUpdate !st0 vdom !acc0 useDom = do
  let
    go :: Maybe state -> state -> acc -> DOM.JSM acc
    go mbPrevSt !st !acc = do
      DOM.syncPoint
      (nodes, vdomDt) <- timeIt (runComponent vdom dispatch mbPrevSt st)
      when (roDebugOutput ro) $
        liftIO (putStrLn ("Vdom generated (" ++ show vdomDt ++ ")"))
      acc' <- useDom acc nodes
      mbF <- getStateUpdate
      case mbF of
        Nothing -> do
          liftIO (putStrLn "No state update received, terminating component loop")
          return acc'
        Just f -> do
          (st', updateDt) <- timeIt (f st)
          liftIO (putStrLn ("State updated (" ++ show updateDt ++ "),  will re render"))
          go (Just st) st' acc'
  go Nothing st0 acc0

installComponentBody ::
     RenderOptions
  -> Dispatch state
  -> DOM.JSM (Maybe (state -> DOM.JSM state))
  -> state
  -> Component' state
  -> DOM.JSM ()
installComponentBody ro dispatch getStateUpdate st0 vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  void $ componentLoop ro dispatch getStateUpdate st0 vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc body mbVdom vdom
    DOM.syncPoint -- force jssaddle stuff
    return (Just (vdom, evts))

installComponentBootstrap ::
     RenderOptions
  -> Dispatch state
  -> DOM.JSM (Maybe (state -> DOM.JSM state))
  -> state
  -> Component' state
  -> DOM.JSM ()
installComponentBootstrap ro dispatch getStateUpdate st0 vdom0 = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  container <- DOM.unsafeCastTo DOM.HTMLDivElement =<< DOM.createElement doc "div"
  DOM.setClassName container "container"
  DOM.appendChild_ body container
  void $ componentLoop ro dispatch getStateUpdate st0 vdom0 Nothing $ \mbVdom vdom -> do
    evts <- renderVirtualDom ro doc container mbVdom vdom
    DOM.syncPoint -- force jssaddle stuff
    return (Just (vdom, evts))

