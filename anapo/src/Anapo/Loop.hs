{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Anapo.Loop
  ( installNodeBody
  , installNode
  , InstallMode(..)
  , ExceptionContext(..)
  , HandledException(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Data.Foldable (traverse_, for_)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Monoid ((<>))
import Control.Exception.Safe (throwIO, tryAsync, SomeException(..), bracket, finally, Exception, Typeable)
import Control.Exception (BlockedIndefinitelyOnSTM)
import Data.IORef (IORef, readIORef, atomicModifyIORef', newIORef, writeIORef)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Monad.State (runStateT, put, get, lift)
import qualified Data.HashMap.Strict as HMS
import Control.Exception.Safe (try)
import Data.List (foldl')
import GHC.Stack
import Control.Concurrent.STM

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM.Node
import qualified GHCJS.DOM.Document as DOM.Document

import qualified Anapo.VDOM as V
import Anapo.Component.Internal
import Anapo.Text (Text, pack)
import Anapo.Logging

#if defined(ghcjs_HOST_OS)
import GHCJS.Concurrent (synchronously)
#else
synchronously :: DOM.JSM a -> DOM.JSM a
synchronously = id
#endif

timeIt :: DOM.JSM a -> DOM.JSM (a, NominalDiffTime)
timeIt m = do
  t0 <- liftIO getCurrentTime
  x <- m
  t1 <- liftIO getCurrentTime
  return (x, diffUTCTime t1 t0)

data DispatchMsg stateRoot = forall props context state. DispatchMsg
  { _dispatchMsgTraverseComp :: AffineTraversal' (Component () () stateRoot) (Component props context state)
  , _dispatchMsgModify :: Text -> Maybe context -> state -> DOM.JSM (state, Rerender)
  , _dispatchCallStack :: CallStack
  }

newtype AnapoException = AnapoException Text
  deriving (Eq, Show, Typeable)
instance Exception AnapoException

data InstallMode =
    IMAppend -- ^ append the node inside the the provided container
  | IMEraseAndAppend -- ^ clear everything in the container and append

data ExceptionContext =
    ECThread
  -- ^ the exception came up in a thread
  | ECUpdatingState
  -- ^ the exception came up while updating the state because of a dispatch
  | ECRendering
  -- ^ the exception came up while rendering

data HandledException =
    HEIgnore -- ^ just continue going as if nothing happened
  | HEShutdown (Node () ()) -- ^ render the given node and shutdown

{-# INLINABLE nodeLoop #-}
nodeLoop :: forall st.
     (forall a. (st -> DOM.JSM a) -> Action () st a)
  -- ^ Function initializing the state.
  -> Node () st
  -- ^ The top-level component
  -> (ExceptionContext -> SomeException -> DOM.JSM HandledException)
  -- ^ How to handle exceptions.
  -- Note that you should should do your own logging of exceptions here,
  -- the loop itself will only print some info messages. This handler
  -- should really not throw!
  -> InstallMode
  -- ^ how to place the node
  -> DOM.Node
  -- ^ where to place the node
  -> DOM.JSM V.RenderedNode
  -- ^ returns the rendered node, when there is nothing left to
  -- do. might never terminate
nodeLoop withState node handleException injectMode root = do
  -- exception tmvar
  excVar :: TMVar SomeException <- liftIO newEmptyTMVarIO
  let handler err = void (atomically (tryPutTMVar excVar err))
  -- dispatch channel
  dispatchChan :: TChan (DispatchMsg st) <- liftIO newTChanIO
  let
    -- 'Nothing' means that no update will ever be received again.
    getErrOrMsg :: IO (Maybe (Either SomeException (DispatchMsg st)))
    getErrOrMsg = do
      -- get the next update or the next exception. we are biased
      -- towards exceptions since we want to give the caller the
      -- chance to exit immediately if there is a failure.
      --
      -- note that it's important to 'takeTMVar', rather than to
      -- 'readTMVar', since the handler might ignore the exception,
      -- so if we did 'readTMVar' we'd end up in an infinite loop.
      mbF :: Either BlockedIndefinitelyOnSTM (Either SomeException (DispatchMsg st)) <-
        tryAsync (atomically (fmap Left (takeTMVar excVar) `orElse` fmap Right (readTChan dispatchChan)))
      case mbF of
        Left{} -> do
          logInfo "got undefinitedly blocked on stm while getting exception or next state update, will stop"
          return Nothing
        Right x -> return (Just x)
  -- set of threads
  tidsRef :: IORef (HashSet ThreadId) <- liftIO (newIORef mempty)
  let
    register m = do
      tid <- myThreadId
      bracket
        (atomicModifyIORef' tidsRef (\tids -> (HS.insert tid tids, ())))
        (\_ -> atomicModifyIORef' tidsRef (\tids -> (HS.delete tid tids, ())))
        (\_ -> m)
  let
    actionEnv :: ActionEnv (Component () () st)
    actionEnv = ActionEnv
      { aeRegisterThread = register
      , aeHandleException = handler
      , aeDispatch = Dispatch (\stack travComp modify -> atomically (writeTChan dispatchChan (DispatchMsg travComp modify stack)))
      }
  let
    actionTrav ::
         AffineTraversal' (Component () () st) (Component props ctx' st')
      -> ActionTraverse (Component () () st) props ctx' st' ctx' st'
    actionTrav travComp = ActionTraverse
      { atToComp = travComp
      , atToState = id
      , atToContext = id
      }
  -- helper to run the component
  let
    runComp ::
         V.Path
      -> AffineTraversal' (Component () () st) (Component props ctx' st')
      -> Component props ctx' st'
      -> props
      -> DOM.JSM V.Node
    runComp path travComp comp props = do
      mbCtx <- liftIO (readIORef (_componentContext comp))
      (vdom, vdomDt) <- timeIt $ unDomM
        (do
          node0 <- _componentNode comp props
          patches <- registerComponent (_componentName comp) (_componentPositions comp) props
          return (foldl' V.addNodeCallback node0 patches))
        actionEnv
        (actionTrav travComp)
        DomEnv
          { domEnvReversePath = reverse path
          , domEnvDirtyPath = False
          , domEnvComponentName = _componentName comp
          }
        mbCtx
        (_componentState comp)
        ()
      DOM.syncPoint
      logDebug ("Vdom generated (" <> pack (show vdomDt) <> ")")
      -- keep in sync with similar code in Anapo.Component.Internal.component
      return $ case _componentFingerprint comp of
        Nothing -> vdom
        Just fprint -> V.setNodeMark vdom (Just fprint)
  -- compute state
  unAction
    (withState $ \st0 -> do
      -- what to do in case of exceptions
      let
        onErr :: DOM.JSM () -> V.RenderedNode -> ExceptionContext -> SomeException -> DOM.JSM ()
        onErr continue rendered ctx err = do
          logInfo ("Got exception " <> pack (show err) <> ", will ask upstream to handle it")
          handled <- handleException ctx err
          case handled of
            HEShutdown comp -> do
              logInfo "Exception handler asked for a shutdown, will render and terminate"
              vdom <- simpleNode () comp
              void (V.reconciliate rendered [] vdom)
            HEIgnore -> do
              logInfo "Exception handler asked for an ignore, will continue"
              continue
      -- main loop
      let
        go ::
             Component () () st
          -- ^ the previous state
          -> V.RenderedNode
          -- ^ the rendered node
          -> DOM.JSM ()
        go compRoot !rendered = do
          fOrErr :: Maybe (Either SomeException (DispatchMsg st)) <- liftIO getErrOrMsg
          case fOrErr of
            Nothing -> do
              logInfo "No state update received, terminating component loop"
              return ()
            Just (Left err) -> do
              onErr (go compRoot rendered) rendered ECThread err
            Just (Right (DispatchMsg travComp modif stack)) -> do
              logDebug (addCallStack stack "About to update state")
              -- traverse to the component using StateT, failing
              -- if we reach anything twice (it'd mean it's not an
              -- AffineTraversal)
              (compOrErr, updateDt) <- timeIt $ try $ runStateT
                (travComp
                  (\comp -> do
                      logDebug ("Visiting component " <> _componentName comp)
                      mbComp <- get
                      case mbComp of
                        Just{} -> do
                          -- fail if we already visited
                          lift $ throwIO $ AnapoException
                            "nodeLoop: visited multiple elements in the affine traversal for component! check if your AffineTraversal are really affine"
                        Nothing -> do
                          mbCtx <- liftIO (readIORef (_componentContext comp))
                          -- run the state update synchronously: both
                          -- because we want it to be done asap, and
                          -- because we want to crash it if there are
                          -- blocking calls
                          (st, rerender) <- DOM.liftJSM $
                            synchronously (modif (_componentName comp) mbCtx (_componentState comp))
                          let comp' = comp{ _componentState = st }
                          put (Just (comp', rerender))
                          return comp')
                  compRoot)
                Nothing
              case compOrErr of
                Left err -> do
                  onErr (go compRoot rendered) rendered ECUpdatingState err
                Right (compRoot', mbComp) -> do
                  logDebug ("State updated (" <> pack (show updateDt) <> "), might re render")
                  case mbComp of
                    Nothing -> do
                      logInfo "The component was not found, not rerendering"
                      go compRoot' rendered
                    Just (comp, rerender) -> do
                      case rerender of
                        UnsafeDontRerender -> do
                          logDebug ("Not rerendering component " <> _componentName comp <> " since the update function returned UnsafeDontRerender")
                          go compRoot' rendered
                        Rerender -> do
                          positions <- liftIO (readIORef (_componentPositions comp))
                          logDebug ("Rendering component " <> _componentName comp <> " at " <> pack (show (HMS.size positions)) <> " positions")
                          -- do not leave half-done DOM in place, run
                          -- everything synchronously
                          --
                          -- important to 'try' wrapping the synchronously: this way
                          -- we'll catch "thread would block" errors.
                          mbErr <- try $ synchronously $ for_ (HMS.toList positions) $ \(pos, props) -> do
                            vdom <- runComp pos travComp comp props
                            V.reconciliate rendered pos vdom
                          case mbErr of
                            Left err -> do
                              onErr (go compRoot rendered) rendered ECRendering err
                            Right () -> do
                              go compRoot' rendered
      tid <- liftIO myThreadId
      finally
        (do
          -- run for the first time
          comp <- newNamedComponent "root" st0 (\() -> node)
          liftIO (writeIORef (_componentContext comp) (Just ()))
          vdom <- runComp [] id comp ()
          -- do this synchronously, too
          rendered0 <- synchronously $ do
            V.render vdom $ \rendered -> do
              case injectMode of
                IMAppend -> return ()
                IMEraseAndAppend -> removeAllChildren root
              DOM.Node.appendChild_ root =<< V.renderedNodeDom rendered
          -- now loop
          go comp rendered0
          return rendered0)
        (liftIO (readIORef tidsRef >>= traverse_ (\tid' -> when (tid /= tid') (killThread tid')))))
    actionEnv
    (actionTrav id)

removeAllChildren :: DOM.Node -> DOM.JSM ()
removeAllChildren node = go
  where
    go = do
      mbChild <- DOM.Node.getFirstChild node
      case mbChild of
        Nothing -> return ()
        Just child -> do
          DOM.Node.removeChild_ node child
          go

{-# INLINABLE installNodeBody #-}
installNodeBody ::
     (forall a. (st -> DOM.JSM a) -> Action () st a)
  -- ^ Function initializing the state.
  -> Node () st
  -- ^ The top-level component
  -> (ExceptionContext -> SomeException -> DOM.JSM HandledException)
  -- ^ How to handle exceptions.
  -- Note that you should should do your own logging of exceptions here,
  -- the loop itself will only print some info messages. This handler
  -- should really not throw!
  -> InstallMode
  -> DOM.JSM ()
installNodeBody getSt handleExceptions excVdom injectMode = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.Document.getBodyUnchecked doc
  void (nodeLoop getSt handleExceptions excVdom injectMode (DOM.toNode body))

{-# INLINABLE installNode #-}
installNode ::
     (DOM.IsNode el)
  => (forall a. (st -> DOM.JSM a) -> Action () st a)
  -- ^ Function initializing the state.
  -> Node () st
  -- ^ The top-level component
  -> (ExceptionContext -> SomeException -> DOM.JSM HandledException)
  -- ^ How to handle exceptions.
  -- Note that you should should do your own logging of exceptions here,
  -- the loop itself will only print some info messages. This handler
  -- should really not throw!
  -> InstallMode
  -> el
  -> DOM.JSM ()
installNode getSt handleExceptions excVdom injectMode container = do
  void (nodeLoop getSt handleExceptions excVdom injectMode (DOM.toNode container))

addCallStack :: CallStack -> Text -> Text
addCallStack stack = case getCallStack stack of
  [] -> id
  (_, SrcLoc{..}) : _ -> \txt -> "[" <> pack srcLocModule <> ":" <> pack (show srcLocStartLine) <> ":" <> pack (show srcLocStartCol) <> "] " <> txt
