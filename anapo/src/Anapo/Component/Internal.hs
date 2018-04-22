{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Note: we use 'Traversal' to keep a cursor to the
-- write end of the state, but really we should use an
-- "affine traversal" which guarantees we have either 0
-- or 1 positions to traverse. See
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>
-- for why affine traversals do not play well with lens.
module Anapo.Component.Internal where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, lens, (^.))
import qualified Control.Lens as Lens
import qualified Control.Lens.Internal.Zoom as Lens
import Control.Monad (ap, when, unless)
import Data.Monoid ((<>), Endo)
import qualified Data.DList as DList
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack, CallStack, callStack)
import Control.Monad.State (StateT(..), MonadState(..), runStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO, MonadUnliftIO, UnliftIO(..))
import Control.Exception.Safe (SomeException, uninterruptibleMask, tryAny)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Monad.Trans (lift)
import Data.DList (DList)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
import Control.Monad.Reader (MonadReader(..))
import qualified Data.Vector as Vec
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM.EventM
import GHC.Fingerprint.Type (Fingerprint)
import Control.Applicative ((<|>))

import qualified Anapo.VDOM as V
import Anapo.Render
import Anapo.Logging
import Anapo.Text (Text, pack)
import qualified Anapo.Text as T
import qualified Anapo.OrderedHashMap as OHM

#if !defined(ghcjs_HOST_OS)
import qualified Language.Javascript.JSaddle as JSaddle
#endif

-- affine traversals
-- --------------------------------------------------------------------

-- | for the components we want affine traversals --
-- traversals which point either to one or to zero elements.
-- however lens currently does not provide them, see
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>.
-- therefore we provide a type synonym for clarity.
type AffineTraversal a b c d = Lens.Traversal a b c d
type AffineTraversal' a b = Lens.Traversal' a b
type AffineFold a b = Lens.Fold a b

-- | to be used with 'AffineTraversal' and 'AffineFold'
{-# INLINE toMaybeOf #-}
toMaybeOf :: (HasCallStack) => Lens.Getting (Endo [a]) s a -> s -> Maybe a
toMaybeOf l x = case Lens.toListOf l x of
  [] -> Nothing
  [y] -> Just y
  _:_ -> error "toMaybeOf: multiple elements returned!"

-- Dispatching and handling
-- --------------------------------------------------------------------

newtype Dispatch stateRoot = Dispatch
  { unDispatch :: forall state context props.
      CallStack ->
      AffineTraversal' stateRoot (Component props context state) ->
      (context -> state -> DOM.JSM (state, V.Rerender)) ->
      IO ()
  }

-- Register / handle
-- --------------------------------------------------------------------

-- we use these two on events and on forks, so that we can handle
-- all exceptions in a central place and so that we don't leave
-- dangling threads

type RegisterThread = IO () -> IO ()
type HandleException = SomeException -> IO ()

-- Action
-- --------------------------------------------------------------------

-- | An action we'll spawn from a component -- for example when an event
-- fires or as a fork in an event
newtype Action context state a = Action
  { unAction ::
         forall rootState compProps compContext compState.
         ActionEnv rootState
      -> ActionTraverse rootState compProps compContext compState context state
      -- we keep this separate from ActionEnv because ActionTraverse
      -- changes all the time, ActionEnv never does.
      -> DOM.JSM a
  }

data ActionEnv rootState = ActionEnv
  { aeRegisterThread :: RegisterThread
  , aeHandleException :: HandleException
  , aeDispatch :: Dispatch rootState
  }

data ActionTraverse rootState compProps compContext compState context state = ActionTraverse
  { atToComp :: AffineTraversal' rootState (Component compProps compContext compState)
  , atToState :: AffineTraversal' compState state
  , atToContext :: AffineFold compContext context
  }

instance Functor (Action context state) where
  {-# INLINE fmap #-}
  fmap f (Action g) = Action $ \env trav -> do
    x <- g env trav
    return (f x)

instance Applicative (Action context state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Action context state) where
  {-# INLINE return #-}
  return x = Action (\_env _trav -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = Action $ \env trav -> do
    x <- unAction ma env trav
    unAction (mf x) env trav

instance MonadIO (Action context state) where
  {-# INLINE liftIO #-}
  liftIO m = Action (\_env _trav -> liftIO m)

#if !defined(ghcjs_HOST_OS)
instance  JSaddle.MonadJSM (Action context state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = Action (\_env _trav -> m)
#endif

instance MonadUnliftIO (Action context state) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Action $ \env trav -> do
    u <- askUnliftIO
    return (UnliftIO (\(Action m) -> unliftIO u (m env trav)))

class (DOM.MonadJSM m) => MonadAction context state m | m -> context, m -> state where
  liftAction :: Action context state a -> m a

instance MonadAction context state (Action context state) where
  {-# INLINE liftAction #-}
  liftAction = id

instance (MonadAction context state m) => MonadAction context state (StateT s m) where
  {-# INLINE liftAction #-}
  liftAction m = lift (liftAction m)

{-# INLINE actZoom #-}
actZoom :: MonadAction context out m => AffineTraversal' out in_ -> Action context in_ a -> m a
actZoom t m =
  liftAction (Action (\env trav -> unAction m env trav{atToState = atToState trav . t}))

{-# INLINE actZoomCtx #-}
actZoomCtx :: MonadAction out state m => AffineFold out in_ -> Action in_ state a -> m a
actZoomCtx t m =
  liftAction (Action (\env trav -> unAction m env trav{atToContext = atToContext trav . t}))

{-# INLINE noContext #-}
noContext :: Lens.Fold a ()
noContext f x = f () *> pure x

{-# INLINE actComponent #-}
actComponent ::
     MonadAction context0 (Component props context state) m
  => Action context state a
  -> m a
actComponent (Action m) = liftAction $ Action $ \env trav ->
  m env trav
    { atToComp = atToComp trav . compState . atToState trav
    , atToState = id
    , atToContext = id
    }

{-# INLINE forkRegistered #-}
forkRegistered :: MonadUnliftIO m => RegisterThread -> HandleException -> m () -> m ThreadId
forkRegistered register handler m = do
  u <- askUnliftIO
  liftIO $ uninterruptibleMask $ \restore -> forkIO $ register $ do
    mbErr <- tryAny (restore (unliftIO u m))
    case mbErr of
      Left err -> do
        tid <- myThreadId
        logError ("Caught exception in registered thread " <> pack (show tid) <> ", will handle it upstream: " <> pack (show err))
        handler err
      Right _ -> return ()

{-# INLINE actFork #-}
actFork ::
     MonadAction context state m
  => Action context state ()
  -> m ThreadId
actFork m =
  liftAction (Action (\env trav -> forkRegistered (aeRegisterThread env) (aeHandleException env) (unAction m env trav)))

newtype DispatchM context0 state0 context state a =
  DispatchM {unDispatchM :: context -> state -> Action context0 state0 (a, state)}

type DispatchM' ctx st = DispatchM ctx st ctx st

instance Functor (DispatchM context0 state0 context state) where
  {-# INLINE fmap #-}
  fmap f (DispatchM g) = DispatchM (\ctx st -> do (x, st') <- g ctx st; return (f x, st'))

instance Applicative (DispatchM context0 state0 context state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (DispatchM context0 state0 context state) where
  {-# INLINE return #-}
  return x = DispatchM (\_ctx st -> return (x, st))

  {-# INLINE (>>=) #-}
  DispatchM mx >>= mf = DispatchM $ \ctx st1 -> do
    (x, st2) <- mx ctx st1
    unDispatchM (mf x) ctx st2

instance MonadReader context (DispatchM context0 state0 context state) where
  {-# INLINE ask #-}
  ask = DispatchM (\ctx st -> return (ctx, st))
  {-# INLINE reader #-}
  reader f = DispatchM (\ctx st -> return (f ctx, st))
  {-# INLINE local #-}
  local f (DispatchM g) = DispatchM (\ctx st -> g (f ctx) st)

instance MonadState state (DispatchM context0 state0 context state) where
  {-# INLINE get #-}
  get = DispatchM (\_ctx st -> return (st, st))
  {-# INLINE put #-}
  put st = DispatchM (\_ctx _st -> return ((), st))
  {-# INLINE state #-}
  state f = DispatchM (\_ctx st -> return (f st))

instance MonadIO (DispatchM context0 state0 context state) where
  {-# INLINE liftIO #-}
  liftIO m = DispatchM (\_ctx st -> do x <- liftIO m; return (x, st))

#if !defined(ghcjs_HOST_OS)
instance JSaddle.MonadJSM (DispatchM context0 state0 context state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = DispatchM (\_ctx st -> do x <- JSaddle.liftJSM' m; return (x, st))
#endif

instance MonadAction context0 state0 (DispatchM context0 state0 context state) where
  {-# INLINE liftAction #-}
  liftAction m = DispatchM (\_ctx st -> do x <- m; return (x, st))

type instance Lens.Magnified (DispatchM ctx0 state0 ctx state) = Lens.Effect (StateT state (Action ctx0 state0))
instance Lens.Magnify (DispatchM ctx0 state0 in_ state) (DispatchM ctx0 state0 out state) in_ out where
  {-# INLINE magnify #-}
  magnify l (DispatchM m) =
    DispatchM (\ctx st -> runStateT (Lens.getEffect (l (\ctx' -> Lens.Effect (StateT (m ctx'))) ctx)) st)

type instance Lens.Zoomed (DispatchM ctx0 state0 ctx state) = Lens.Focusing (Action ctx0 state0)
instance Lens.Zoom (DispatchM ctx0 state0 ctx in_) (DispatchM ctx0 state0 ctx out) in_ out where
  {-# INLINE zoom #-}
  zoom l (DispatchM m) =
    DispatchM (\ctx st -> Lens.unfocusing (l (\st' -> Lens.Focusing (m ctx st')) st))

{-# INLINE dispatch #-}
dispatch ::
     (HasCallStack, MonadAction context state m)
  => DispatchM context state context state ()
  -> m ()
dispatch (DispatchM m) =
  liftAction $ Action $ \env trav ->
    liftIO $ unDispatch
      (aeDispatch env)
      callStack
      (atToComp trav)
      (\ctx st -> case toMaybeOf (atToContext trav) ctx of
          Nothing -> return (st, V.UnsafeDontRerender)
          Just ctx' -> do
            st' <- atToState trav (\st' -> fmap snd (unAction (m ctx' st') env trav)) st
            return (st', V.Rerender))

-- | Variant of dispatch that lets you decide whether the state update
-- will trigger a rerender or not. Obviously must be used with care,
-- since nothing will check that the UnsafeDontRerender is valid
{-# INLINE dispatchRerender #-}
dispatchRerender ::
     (HasCallStack, MonadAction context state m)
  => DispatchM context state context state V.Rerender
  -> m ()
dispatchRerender (DispatchM m) =
  liftAction $ Action $ \env trav ->
    liftIO $ unDispatch
      (aeDispatch env)
      callStack
      (atToComp trav)
      (\ctx st -> case toMaybeOf (atToContext trav) ctx of
          Nothing -> return (st, V.UnsafeDontRerender)
          Just ctx' -> do
            (st', rerender) <- runStateT
              (atToState trav
                (\st' -> do
                  (rerender, st'') <- lift (unAction (m ctx' st') env trav)
                  put rerender
                  return st'')
                st)
              V.UnsafeDontRerender
            return (st', rerender))

{-# INLINE askRegisterThread #-}
askRegisterThread :: (MonadAction context state m) => m RegisterThread
askRegisterThread = liftAction (Action (\env _trav -> return (aeRegisterThread env)))

{-# INLINE askHandleException #-}
askHandleException :: (MonadAction context state m) => m HandleException
askHandleException = liftAction (Action (\env _trav -> return (aeHandleException env)))

-- Monad
-- --------------------------------------------------------------------

data DomEnv stateRoot = DomEnv
  { domEnvReversePath :: [VDomPathSegment]
  -- ^ this is stored in _reverse_ order. IMPORTANT: updatedomDirtyPath
  -- every time you modify this.
  , domEnvDirtyPath :: Bool
  -- ^ this is whether we have added any path since the last component.
  -- it's used solely to prevent people placing a component on the same
  -- path, which will make them step on each other toes.
  , domEnvPrevState :: Maybe stateRoot
  }

newtype DomM dom context state a = DomM
  { unDomM ::
         forall rootState compProps compContext compState.
         ActionEnv rootState
      -> ActionTraverse rootState compProps compContext compState context state
      -> DomEnv rootState
      -> context
      -> state
      -> dom
      -> DOM.JSM (dom, a)
  }

data DomState = DomState
  { domStateLength :: Int
  , domStateDom :: ~(DList (V.Node V.SomeVDomNode))
  }

newtype KeyedDomState = KeyedDomState (DList (Text, V.Node V.SomeVDomNode))

newtype MapDomState = MapDomState (DList (Text, V.Node V.SomeVDomNode))

type Dom ctx st = DomM DomState ctx st ()
type Dom' ctx st a = DomM DomState ctx st a
type KeyedDom ctx st = DomM KeyedDomState ctx st ()
type KeyedDom' ctx st a = DomM KeyedDomState ctx st a
type MapDom ctx st = DomM MapDomState ctx st ()
type MapDom' ctx st a = DomM MapDomState ctx st a
type Node ctx st = DomM () ctx st (V.Node V.SomeVDomNode)
type Node' ctx st a = DomM () ctx st (V.Node V.SomeVDomNode, a)

instance MonadIO (DomM dom ctx st) where
  {-# INLINE liftIO #-}
  liftIO m = DomM $ \_actEnv _acTrav _anEnv _ctx _st dom -> do
    x <- liftIO m
    return (dom, x)

#if !defined(ghcjs_HOST_OS)
instance JSaddle.MonadJSM (DomM dom ctx st) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = DomM $ \_actEnv _acTrav _anEnv _ctx _st dom -> do
    x <- m
    return (dom, x)
#endif

instance Functor (DomM dom ctx st) where
  {-# INLINE fmap #-}
  fmap f (DomM g) = DomM $ \acEnv acTrav aeEnv ctx st dom -> do
    (dom', x) <- g acEnv acTrav aeEnv ctx st dom
    return (dom', f x)

instance Applicative (DomM dom ctx st) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (DomM dom ctx st) where
  {-# INLINE return #-}
  return x = DomM (\_acEnv _acTrav _aeEnv _ctx _st dom -> return (dom, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = DomM $ \acEnv acTrav anEnv ctx st dom0 -> do
    (dom1, x) <- unDomM ma acEnv acTrav anEnv ctx st dom0
    (dom2, y) <- unDomM (mf x) acEnv acTrav anEnv ctx st dom1
    return (dom2, y)

instance (a ~ ()) => Monoid (DomM dom ctx st a) where
  {-# INLINE mempty #-}
  mempty = return ()
  {-# INLINE mappend #-}
  mappend = (>>)

instance MonadAction ctx st (DomM dom ctx st) where
  {-# INLINE liftAction #-}
  liftAction (Action f) = DomM $ \acEnv acTrav _anEnv _ctx _st dom -> do
    x <- f acEnv acTrav
    return (dom, x)

instance MonadReader st (DomM dom ctx st) where
  {-# INLINE ask #-}
  ask = DomM (\_acEnv _acTrav _anEnv _ctx st dom -> return (dom, st))
  {-# INLINE local #-}
  local f m = DomM $ \acEnv acTrav anEnv ctx st dom -> do
    unDomM m acEnv acTrav anEnv ctx (f st) dom

{-# INLINE askPreviousState #-}
askPreviousState :: DomM dom ctx st (Maybe st)
askPreviousState =
  DomM $ \_acEnv acTrav anEnv _ctx _st dom ->
    return
      ( dom
      , toMaybeOf (atToComp acTrav . compState . atToState acTrav) =<< domEnvPrevState anEnv
      )

{-# INLINE askContext #-}
askContext :: DomM dom ctx st ctx
askContext = DomM (\_acEnv _acTrav _anEnv ctx _st dom -> return (dom, ctx))

{-# INLINE viewContext #-}
viewContext :: Lens.Getting a ctx a -> DomM dom ctx st a
viewContext l = DomM (\_acEnv _acTrav _anEnv ctx _st dom -> return (dom, ctx ^. l))

{-# INLINE zoomL #-}
zoomL :: Lens' out in_ -> DomM dom ctx in_ a -> DomM dom ctx out a
zoomL l m = DomM $ \acEnv acTrav anEnv ctx st dom ->
  unDomM m
    acEnv
    acTrav{ atToState = atToState acTrav . l }
    anEnv
    ctx
    (view l st)
    dom

{-# INLINE zoomT #-}
zoomT ::
     HasCallStack
  => in_
  -> AffineTraversal' out in_
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> DomM dom ctx in_ a
  -> DomM dom ctx out a
zoomT st l m = DomM $ \acEnv acTrav anEnv ctx _st dom ->
  unDomM m
    acEnv
    acTrav{ atToState = atToState acTrav . l }
    anEnv
    ctx
    st
    dom

{-
{-# INLINE zoomCtxL #-}
zoomCtxL :: Lens' out in_ -> DomM dom in_ st a -> DomM dom out st a
zoomCtxL l m = DomM $ \acEnv acTrav anEnv curr dom ->
  unDomM m
    acEnv
    acTrav{ atToContext = atToContext acTrav . l }
    anEnv
    curr{ domCurrentContext = view l (domCurrentContext curr) }
    dom
-}

{-# INLINE zoomCtxF #-}
zoomCtxF ::
     HasCallStack
  => in_
  -> AffineFold out in_
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> DomM dom in_ st a
  -> DomM dom out st a
zoomCtxF ctx l m = DomM $ \acEnv acTrav anEnv _ctx st dom ->
  unDomM m
    acEnv
    acTrav{ atToContext = atToContext acTrav . l }
    anEnv
    ctx
    st
    dom

{-# INLINE zoomCtxL #-}
zoomCtxL ::
     HasCallStack
  => Lens' out in_
  -> DomM dom in_ st a
  -> DomM dom out st a
zoomCtxL l m = DomM $ \acEnv acTrav anEnv ctx st dom ->
  unDomM m
    acEnv
    acTrav{ atToContext = atToContext acTrav . l }
    anEnv
    (view l ctx)
    st
    dom

-- to manipulate nodes
-- --------------------------------------------------------------------

{-# INLINE willMount #-}
willMount :: (el -> Action ctx st ()) -> NodePatch el ctx st
willMount = NPWillMount

{-# INLINE didMount #-}
didMount :: (el -> Action ctx st ()) -> NodePatch el ctx st
didMount = NPDidMount

{-# INLINE willPatch #-}
willPatch :: (el -> Action ctx st ()) -> NodePatch el ctx st
willPatch = NPWillPatch

{-# INLINE didPatch #-}
didPatch :: (el -> Action ctx st ()) -> NodePatch el ctx st
didPatch = NPDidPatch

{-# INLINE willRemove #-}
willRemove :: (el -> Action ctx st ()) -> NodePatch el ctx st
willRemove = NPWillRemove

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: Node ctx st -> Dom ctx st
n getNode = DomM $ \acEnv acTrav anEnv ctx st dom -> do
  (_, nod) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSNormal (domStateLength dom) : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return
    ( dom
        { domStateLength = domStateLength dom + 1
        , domStateDom = domStateDom dom <> DList.singleton nod
        }
    , ()
    )

{-# INLINE n' #-}
n' :: Node' ctx st a -> Dom' ctx st a
n' getNode = DomM $ \acEnv acTrav anEnv ctx st dom -> do
  (_, (nod, x)) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSNormal (domStateLength dom) : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return
    ( dom
        { domStateLength = domStateLength dom + 1
        , domStateDom = domStateDom dom <> DList.singleton nod
        }
    , x
    )

{-# INLINE key #-}
key :: Text -> Node ctx st -> KeyedDom ctx st
key k getNode = DomM $ \acEnv acTrav anEnv ctx st (KeyedDomState dom) -> do
  (_, nod) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return (KeyedDomState (dom <> DList.singleton (k, nod)), ())

{-# INLINE key' #-}
key' :: Text -> Node' ctx st a -> KeyedDom' ctx st a
key' k getNode = DomM $ \acEnv acTrav anEnv ctx st (KeyedDomState dom) -> do
  (_, (nod, x)) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return (KeyedDomState (dom <> DList.singleton (k, nod)), x)

{-# INLINE ukey #-}
ukey :: Text -> Node ctx st -> MapDom ctx st
ukey k getNode = DomM $ \acEnv acTrav anEnv ctx st (MapDomState dom) -> do
  (_, nod) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return (MapDomState (dom <> DList.singleton (k, nod)), ())

{-# INLINE ukey' #-}
ukey' :: Text -> Node' ctx st a -> MapDom' ctx st a
ukey' k getNode = DomM $ \acEnv acTrav anEnv ctx st (MapDomState dom) -> do
  (_, (nod, x)) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = VDPSKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  return (MapDomState (dom <> DList.singleton (k, nod)), x)

{-# INLINE text #-}
text :: Text -> Node ctx st
text txt = return $ V.Node
  { V.nodeBody = V.SomeVDomNode $ V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBText txt
      , V.vdomCallbacks = mempty
      , V.vdomWrap = DOM.Text
      }
  , V.nodeChildren = Nothing
  }

instance (el ~ DOM.Text) => IsString (Node ctx st) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE rawNode #-}
rawNode ::
     (DOM.IsNode el)
  => (DOM.JSVal -> el) -> el
  -> [NodePatch el ctx st]
  -> Node ctx st
rawNode wrap x patches = do
  node <- patchNode patches
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBRawNode x
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
  return V.Node
    { V.nodeBody = V.SomeVDomNode node
    , V.nodeChildren = Nothing
    }


-- TODO this causes linking errors, sometimes. bizzarely, the linking
-- errors seem to happen only if a closure is formed -- e.g. if we
-- define the function as
--
-- @
-- marked shouldRerender ptr = deRefStaticPtr ptr
-- @
--
-- things work, but if we define it as
--
-- @
-- marked shouldRerender ptr = do
--   nod <- deRefStaticPtr ptr
--   return nod
-- @
--
-- they don't. the errors look like this:
--
-- @
-- dist/build/test-app/test-app-tmp/Anapo/TestApps/YouTube.dyn_o: In function `hs_spt_init_AnapoziTestAppsziYouTube':
-- ghc_18.c:(.text.startup+0x3): undefined reference to `r19F9_closure'
-- @
--
-- at call site (see for example Anapo.TestApps.YouTube)
{-# INLINE marked #-}
marked ::
     (Maybe state -> props -> state -> V.Rerender)
  -> StaticPtr (props -> Node context state) -> props -> Node context state
marked shouldRerender ptr props = DomM $ \acEnv acTrav domEnv ctx st dom -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender
        (toMaybeOf (atToComp acTrav . compState . atToState acTrav) =<< domEnvPrevState domEnv)
        props
        st
  (_, V.Node (V.SomeVDomNode nod) children) <-
    unDomM (deRefStaticPtr ptr props) acEnv acTrav domEnv ctx st dom
  return ((), V.Node (V.SomeVDomNode nod{ V.vdomMark = Just (V.Mark fprint rer) }) children)

-- Utilities to quickly create nodes
-- --------------------------------------------------------------------

data SomeEventAction el ctx st = forall e. (DOM.IsEvent e) =>
  SomeEventAction (DOM.EventM.EventName el e) (el -> e -> Action ctx st ())
newtype UnsafeRawHtml = UnsafeRawHtml Text

data NodePatch el ctx st =
    NPWillMount (el -> Action ctx st ())
  | NPDidMount (el -> Action ctx st ())
  | NPWillPatch (el -> Action ctx st ())
  | NPDidPatch (el -> Action ctx st ())
  | NPWillRemove (el -> Action ctx st ())
  | NPStyle V.StylePropertyName V.StyleProperty
  | NPAttribute V.AttributeName (DOM.JSM V.AttributeBody)
  | NPProperty V.ElementPropertyName (DOM.JSM V.ElementProperty)
  | NPEvent (SomeEventAction el ctx st)

class IsElementChildren a ctx st where
  elementChildren :: HasCallStack => a -> DomM () ctx st (V.Children V.SomeVDomNode)
instance IsElementChildren () ctx st where
  {-# INLINE elementChildren #-}
  elementChildren _ = return (V.CNormal mempty)
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM DomState ctx1 st1 a) ctx2 st2 where
  {-# INLINE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    (DomState _ dom, _) <- f acEnv acTrav anEnv ctx st (DomState 0 mempty)
    return ((), V.CNormal (Vec.fromList (DList.toList dom)))
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM KeyedDomState ctx1 st1 a) ctx2 st2 where
  {-# INLINE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    (KeyedDomState dom, _) <- f acEnv acTrav anEnv ctx st (KeyedDomState mempty)
    return ((), V.CKeyed (OHM.fromList (DList.toList dom)))
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM MapDomState ctx1 st1 a) ctx2 st2 where
  {-# INLINE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    (MapDomState dom, _) <- f acEnv acTrav anEnv ctx st (MapDomState mempty)
    let kvs = DList.toList dom
    let numKvs = length kvs
    let hm = HMS.fromList kvs
    if numKvs /= HMS.size hm
      then error "duplicate keys when building map vdom!"
      else return ((), V.CMap (HMS.fromList kvs))
instance IsElementChildren UnsafeRawHtml ctx st where
  {-# INLINE elementChildren #-}
  elementChildren (UnsafeRawHtml txt) = return (V.CRawHtml txt)

{-# INLINE patchNode #-}
patchNode ::
     (HasCallStack, DOM.IsNode el)
  => [NodePatch el ctx state] -> V.VDomNode el -> DomM () ctx state (V.VDomNode el)
patchNode patches00 node00 = do
  u <- liftAction askUnliftIO
  let
    modifyCallbacks body f =
      body{ V.vdomCallbacks = f (V.vdomCallbacks body) }
  let
    modifyElement ::
         V.VDomNode el
      -> ((DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => V.Element el -> V.Element el)
      -> V.VDomNode el
    modifyElement body f = case V.vdomBody body of
      V.VDBElement e -> body{ V.vdomBody = V.VDBElement (f e) }
      V.VDBText{} -> error "got patch requiring an element body, but was NBText"
      V.VDBRawNode{} -> error "got patch requiring an element body, but was NBRawNode"
  let
    go !node = \case
      [] -> return node
      patch : patches -> case patch of
        NPWillMount cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksWillMount = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPDidMount cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksDidMount = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPWillPatch cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksWillPatch = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPDidPatch cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksDidPatch = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPWillRemove cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksWillRemove = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPStyle styleName styleBody -> go
          (modifyElement node $ \vel -> vel
            { V.elementStyle =
                HMS.insert styleName styleBody (V.elementStyle vel)
            })
          patches
        NPAttribute attrName attrBody -> do
          attrBodyVal <- DOM.liftJSM attrBody
          go
            (modifyElement node $ \vel -> vel
              { V.elementAttributes =
                  HMS.insert attrName attrBodyVal (V.elementAttributes vel)
              })
            patches
        NPProperty propName propBody -> do
          propBodyVal <- DOM.liftJSM propBody
          go
            (modifyElement node $ \vel -> vel
              { V.elementProperties =
                  HMS.insert propName propBodyVal (V.elementProperties vel)
              })
            patches
        NPEvent (SomeEventAction evName evListener) -> go
          (modifyElement node $ \vel -> vel
            { V.elementEvents = DList.snoc
                (V.elementEvents vel)
                (V.SomeEvent evName $ \e ev ->
                  liftIO (unliftIO u (evListener e ev)))
            })
          patches
  go node00 patches00

{-# INLINE el #-}
el ::
     ( IsElementChildren a ctx st
     , DOM.IsElement el, DOM.IsElementCSSInlineStyle el
     , HasCallStack
     )
  => V.ElementTag
  -> (DOM.JSVal -> el)
  -> [NodePatch el ctx st]
  -> a
  -> Node ctx st
el tag wrap patches isChildren = do
  children <- elementChildren isChildren
  vdom <- patchNode patches
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBElement V.Element
          { V.elementTag = tag
          , V.elementProperties = mempty
          , V.elementStyle = mempty
          , V.elementEvents = mempty
          , V.elementAttributes = mempty
          }
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
  return V.Node
    { V.nodeBody = V.SomeVDomNode vdom
    , V.nodeChildren = Just children
    }

-- Properties
-- --------------------------------------------------------------------

{-# INLINE property #-}
property :: DOM.ToJSVal a => Text -> a -> NodePatch el context state
property k v = NPProperty k (DOM.toJSVal v)

{-# INLINE style #-}
style :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> NodePatch el context state
style = NPStyle

{-# INLINE rawAttribute #-}
rawAttribute :: DOM.ToJSVal a => Text -> a -> NodePatch el context state
rawAttribute k v = NPAttribute k (DOM.toJSVal v)

{-# INLINE attribute #-}
-- | Note: for all standard HTML attributes (e.g. class, src, etc.), you
-- should use @property@, not @attribute@.
attribute :: Text -> Text -> NodePatch el context state
attribute k v = NPAttribute k (DOM.toJSVal v)

{-# INLINE onEvent #-}
onEvent ::
     (DOM.IsEventTarget t, DOM.IsEvent e, DOM.MonadJSM m, MonadUnliftIO m)
  => t -> DOM.EventM.EventName t e -> (e -> m ()) -> m (DOM.JSM ())
onEvent el_ evName f = do
  u <- askUnliftIO
  DOM.liftJSM $ DOM.EventM.on el_ evName $ do
    ev <- ask
    liftIO (unliftIO u (f ev))

-- simple rendering
-- --------------------------------------------------------------------

simpleNode :: forall state. state -> Node () state -> DOM.JSM (V.Node V.SomeVDomNode)
simpleNode st node0 = do
  comp <- newComponent st (\() -> node0)
  liftIO (writeIORef (_componentContext comp) (Just ()))
  (_, vdom) <- unDomM
    (do
      node <- _componentNode comp ()
      V.forSomeNodeBody node $ \node' -> do
        patches <- registerComponent (_componentPositions comp) ()
        patchNode patches node')
    ActionEnv
      { aeRegisterThread = \_ -> fail "Trying to register a thread from the simpleRenderNode"
      , aeHandleException = \_ -> fail "Trying to handle an exception from the simpleRenderNode"
      , aeDispatch = Dispatch (\_ _ _ -> fail "Trying to dispatch from the simpleRenderNode")
      }
    ActionTraverse
      { atToComp = id
      , atToState = id
      , atToContext = id
      }
    DomEnv
      { domEnvReversePath = []
      , domEnvDirtyPath = False
      , domEnvPrevState = Nothing
      }
    ()
    st
    ()
  return vdom

-- when we want a quick render of a component, e.g. inside a raw node.
-- any attempt to use dispatch will result in an exception; e.g. this
-- will never redraw anything, it's just to quickly draw some elements
simpleRenderNode :: state -> Node () state -> DOM.JSM DOM.Node
simpleRenderNode st node = do
  vdom <- simpleNode st node
  renderVirtualDom vdom (return . renderedVDomNodeDom . V.nodeBody)

-- utils
-- --------------------------------------------------------------------

type UnliftJSM = UnliftIO

{-# INLINE askUnliftJSM #-}
askUnliftJSM :: (MonadUnliftIO m) => m (UnliftJSM m)
askUnliftJSM = askUnliftIO

{-# INLINE unliftJSM #-}
unliftJSM :: (DOM.MonadJSM n) => UnliftJSM m -> m a -> n a
unliftJSM u m = DOM.liftJSM (liftIO (unliftIO u m))

{-# INLINE actionUnliftJSM #-}
actionUnliftJSM :: (MonadAction context state m) => m (UnliftJSM (Action context state))
actionUnliftJSM = liftAction askUnliftJSM

-- Components
-- --------------------------------------------------------------------

data Component props ctx st = Component
  { _componentState :: st
  , _componentNode :: props -> Node ctx st
  , _componentPositions :: IORef (HMS.HashMap VDomPath props)
  , _componentContext :: IORef (Maybe ctx)
  , _componentFingerprint :: Maybe Fingerprint
  , _componentName :: Text
  -- ^ just for debugging
  }

newtype ComponentToken props ctx st = ComponentToken (IORef (Maybe ctx))

{-# INLINE compState #-}
compState :: Lens' (Component props context state) state
compState = lens _componentState (\comp st -> comp{ _componentState = st })

{-# INLINE compNode #-}
compNode :: Lens' (Component props context state) (props -> Node context state)
compNode = lens _componentNode (\comp st -> comp{ _componentNode = st })

newNamedComponent ::
     MonadIO m
  => Text
  -> state
  -> (props -> Node context state)
  -> m (Component props context state)
newNamedComponent name st node = do
  posRef <- liftIO (newIORef mempty)
  ctxRef <- liftIO (newIORef Nothing)
  return (Component st node posRef ctxRef Nothing name)

-- | this is useful since a marked component will never be patched with
-- another component -- a new redraw will always retrigger, which comes
-- in handy when attaching specific actions using didMount / willRemove
-- and friends.
newMarkedComponent ::
     MonadIO m
  => Text
  -> state
  -> StaticPtr (props -> Node context state)
  -> m (Component props context state)
newMarkedComponent name st node = do
  posRef <- liftIO (newIORef mempty)
  ctxRef <- liftIO (newIORef Nothing)
  return (Component st (deRefStaticPtr node) posRef ctxRef (Just (staticKey node)) name)

newComponent ::
     MonadIO m
  => state
  -> (props -> Node context state)
  -> m (Component props context state)
newComponent = newNamedComponent "<no-name>"

newNamedComponent_ ::
     MonadIO m
  => Text
  -> st
  -> Node ctx st
  -> m (Component () ctx st)
newNamedComponent_ name st comp = newNamedComponent name st (\() -> comp)

newComponent_ ::
     MonadIO m
  => st
  -> Node ctx st
  -> m (Component () ctx st)
newComponent_ st comp = newComponent st (\() -> comp)

{-# INLINE registerComponent #-}
registerComponent :: IORef (HMS.HashMap VDomPath props) -> props -> DomM dom a b [NodePatch el ctx st]
registerComponent ref props = DomM $ \_acEnv _acTrav anEnv _ctx _st dom -> do
  let add = \_ -> do
        liftIO (modifyIORef' ref (HMS.insert (reverse (domEnvReversePath anEnv)) props))
  let remove = \_ -> do
        liftIO (modifyIORef' ref (HMS.delete (reverse (domEnvReversePath anEnv))))
  return
    ( dom
    , [ NPWillMount add
      , NPWillPatch remove
      , NPDidPatch add
      , NPWillRemove remove
      ]
    )

{-# INLINE initComponent #-}
initComponent :: Component props ctx st -> ctx -> DomM dom ctx0 st0 (ComponentToken props ctx st)
initComponent comp ctx = liftIO $ do
  writeIORef (_componentContext comp) (Just ctx)
  return (ComponentToken (_componentContext comp))

{-# INLINE component #-}
component :: props -> ComponentToken props ctx st -> Node ctx0 (Component props ctx st)
component props (ComponentToken tok) = do
  (node, pos, mbFprint) <- DomM $ \acEnv acTrav anEnv _ctx comp dom -> do
    let name = _componentName comp
    when (_componentContext comp /= tok) $
      error (T.unpack name <> ": Initialized component does not match state component!")
    unless (domEnvDirtyPath anEnv) $
      error (T.unpack name <> ": Trying to insert component immediately inside another component at path " <> show (reverse (domEnvReversePath anEnv)) <> ", please wrap the inner component in a node.")
    mbCtx <- liftIO (readIORef (_componentContext comp))
    ctx <- case mbCtx of
      Nothing -> error (T.unpack name <> ": Context not initialized!")
      Just ctx -> return ctx
    (dom', node) <- unDomM
      (_componentNode comp props)
      acEnv
      acTrav
        { atToComp = atToComp acTrav . compState . atToState acTrav
        , atToState = id
        , atToContext = id
        }
      anEnv
        { domEnvDirtyPath = False }
      ctx
      (_componentState comp)
      dom
    return (dom', (node, _componentPositions comp, _componentFingerprint comp))
  V.forSomeNodeBody node $ \node' -> do
    patches <- registerComponent pos props
    node'' <- patchNode patches node'
    return node''{ V.vdomMark = V.vdomMark node'' <|> fmap (\fprint -> V.Mark fprint V.Rerender) mbFprint }

{-# INLINE component_ #-}
component_ :: forall ctx0 ctx st. ctx -> Node ctx0 (Component () ctx st)
component_ ctx = do
  comp <- ask
  tok <- initComponent comp ctx
  component () tok

{-# INLINE componentL_ #-}
componentL_ :: Lens' out (Component () ctx state) -> ctx -> Node ctx0 out
componentL_ l props = zoomL l (component_ props)

{-# INLINE componentT_ #-}
componentT_ ::
     HasCallStack
  => Component () ctx state
  -> AffineTraversal' out (Component () ctx state)
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> ctx
  -> Node ctx0 out
componentT_ st l props = zoomT st l (component_ props)

{-# INLINE componentL #-}
componentL :: Lens' out (Component props ctx state) -> props -> ComponentToken props ctx state -> Node ctx0 out
componentL l props tok = zoomL l (component props tok)

{-# INLINE componentT #-}
componentT ::
     HasCallStack
  => Component props ctx state
  -> AffineTraversal' out (Component props ctx state)
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> props
  -> ComponentToken props ctx state
  -> Node ctx0 out
componentT st l props tok = zoomT st l (component props tok)
