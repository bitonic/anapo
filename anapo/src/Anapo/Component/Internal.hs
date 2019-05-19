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
import Control.Lens (Lens', view, lens)
import qualified Control.Lens as Lens
import qualified Control.Lens.Internal.Zoom as Lens
import Control.Monad (ap, when, unless, void)
import Data.Monoid ((<>), Endo)
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack, CallStack, callStack)
import Control.Monad.State (StateT(..), MonadState(..), runStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO, MonadUnliftIO, UnliftIO(..))
import Control.Exception.Safe (SomeException, uninterruptibleMask, tryAny)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Monad.Trans (lift)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
import Control.Monad.Reader (MonadReader(..))
import qualified GHCJS.DOM.Types as DOM
import Data.List (foldl')
import Data.Foldable (for_)
import qualified Language.Javascript.JSaddle as JS
import qualified Data.Semigroup as Semigroup
import GHC.Prim (coerce)

import qualified Anapo.VDOM as V
import Anapo.Logging
import Anapo.Text (Text, pack)
import qualified Anapo.Text as T

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
{-# INLINABLE toMaybeOf #-}
toMaybeOf :: (HasCallStack) => Lens.Getting (Endo [a]) s a -> s -> Maybe a
toMaybeOf l x = case Lens.toListOf l x of
  [] -> Nothing
  [y] -> Just y
  _:_ -> error "toMaybeOf: multiple elements returned!"

-- Dispatching and handling
-- --------------------------------------------------------------------

data Rerender = Rerender | UnsafeDontRerender
  deriving (Eq, Show)

instance Semigroup.Semigroup Rerender where
  Rerender <> _ = Rerender
  _ <> Rerender = Rerender
  _ <> _ = UnsafeDontRerender

instance Monoid Rerender where
  mempty = UnsafeDontRerender
  mappend = (Semigroup.<>)

newtype Dispatch stateRoot = Dispatch
  { unDispatch :: forall state context props.
      CallStack ->
      AffineTraversal' stateRoot (Component props context state) ->
      (Text -> Maybe context -> state -> DOM.JSM (state, Rerender)) ->
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
instance  JS.MonadJSM (Action context state) where
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
noContext f x = x <$ f ()

{-# INLINE noState #-}
noState :: AffineTraversal' a ()
noState f x = x <$ f ()

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

{-# INLINABLE forkRegistered #-}
forkRegistered :: MonadUnliftIO m => RegisterThread -> HandleException -> m () -> m ThreadId
forkRegistered register handler m = do
  u <- askUnliftIO
  liftIO $ uninterruptibleMask $ \restore -> forkIO $ register $ do
    mbErr <- tryAny (restore (unliftIO u m))
    case mbErr of
      Left err -> do
        tid <- myThreadId
        logWarn ("Caught exception in registered thread " <> pack (show tid) <> ", will handle it upstream: " <> pack (show err))
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
  DispatchM {unDispatchM :: Text -> Maybe context -> state -> Action context0 state0 (a, state)}

type DispatchM' ctx st = DispatchM ctx st ctx st

instance Functor (DispatchM context0 state0 context state) where
  {-# INLINE fmap #-}
  fmap f (DispatchM g) = DispatchM $ \compName ctx st ->
    do (x, st') <- g compName ctx st; return (f x, st')

instance Applicative (DispatchM context0 state0 context state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (DispatchM context0 state0 context state) where
  {-# INLINE return #-}
  return x = DispatchM (\_compName _ctx st -> return (x, st))

  {-# INLINE (>>=) #-}
  DispatchM mx >>= mf = DispatchM $ \compName ctx st1 -> do
    (x, st2) <- mx compName ctx st1
    unDispatchM (mf x) compName ctx st2

instance MonadReader context (DispatchM context0 state0 context state) where
  {-# INLINE ask #-}
  ask = DispatchM (\compName ctx st -> return (assertContext compName ctx, st))
  {-# INLINE reader #-}
  reader f = DispatchM (\compName ctx st -> return (f (assertContext compName ctx), st))
  {-# INLINE local #-}
  local f (DispatchM g) = DispatchM (\compName ctx st -> g compName (f <$> ctx) st)

instance MonadState state (DispatchM context0 state0 context state) where
  {-# INLINE get #-}
  get = DispatchM (\_compName _ctx st -> return (st, st))
  {-# INLINE put #-}
  put st = DispatchM (\_compName _ctx _st -> return ((), st))
  {-# INLINE state #-}
  state f = DispatchM (\_compName _ctx st -> return (f st))

instance MonadIO (DispatchM context0 state0 context state) where
  {-# INLINE liftIO #-}
  liftIO m = DispatchM (\_compName _ctx st -> do x <- liftIO m; return (x, st))

#if !defined(ghcjs_HOST_OS)
instance JS.MonadJSM (DispatchM context0 state0 context state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = DispatchM (\_compName _ctx st -> do x <- JS.liftJSM' m; return (x, st))
#endif

instance MonadAction context0 state0 (DispatchM context0 state0 context state) where
  {-# INLINE liftAction #-}
  liftAction m = DispatchM (\_compName _ctx st -> do x <- m; return (x, st))

type instance Lens.Magnified (DispatchM ctx0 state0 ctx state) =
  Lens.Effect (StateT state (Action ctx0 state0))
instance Lens.Magnify (DispatchM ctx0 state0 in_ state) (DispatchM ctx0 state0 out state) in_ out where
  {-# INLINE magnify #-}
  magnify l (DispatchM m) =
    DispatchM $ \compName mbCtx st -> case mbCtx of
      Nothing -> m compName Nothing st
      Just ctx ->
        runStateT (Lens.getEffect (l (\ctx' -> Lens.Effect (StateT (m compName (Just ctx')))) ctx)) st

type instance Lens.Zoomed (DispatchM ctx0 state0 ctx state) = Lens.Focusing (Action ctx0 state0)
instance Lens.Zoom (DispatchM ctx0 state0 ctx in_) (DispatchM ctx0 state0 ctx out) in_ out where
  {-# INLINE zoom #-}
  zoom l (DispatchM m) =
    DispatchM (\compName ctx st -> Lens.unfocusing (l (\st' -> Lens.Focusing (m compName ctx st')) st))

{-# INLINABLE dispatch #-}
dispatch ::
     (HasCallStack, MonadAction context state m)
  => DispatchM context state context state ()
  -> m ()
dispatch m = dispatchRerender (Rerender <$ m)

-- | Variant of dispatch that lets you decide whether the state update
-- will trigger a rerender or not. Obviously must be used with care,
-- since nothing will check that the UnsafeDontRerender is valid
{-# INLINABLE dispatchRerender #-}
dispatchRerender ::
     (HasCallStack, MonadAction context state m)
  => DispatchM context state context state Rerender
  -> m ()
dispatchRerender (DispatchM m) =
  liftAction $ Action $ \env trav ->
    liftIO $ unDispatch
      (aeDispatch env)
      callStack
      (atToComp trav)
      (\compName mbCtx st -> do
          -- if we do not have a context, just execute the action anyway
          -- but with no context. we cannot know if the traversal would
          -- have succeeded since we do not have the context yet, and
          -- at the same time we do not want to skip possibly required
          -- updates, so this is the best we can do.
          let continueWithCtx ctx = do
                (st', rerender) <- runStateT
                  (atToState trav
                    (\st' -> do
                      (rerender, st'') <- lift (unAction (m compName ctx st') env trav)
                      put rerender
                      return st'')
                    st)
                  UnsafeDontRerender
                return (st', rerender)
          case mbCtx of
            Nothing -> continueWithCtx Nothing
            Just ctx -> do
              case toMaybeOf (atToContext trav) ctx of
                Nothing -> return (st, UnsafeDontRerender)
                Just ctx' -> continueWithCtx (Just ctx'))

{-# INLINE askRegisterThread #-}
askRegisterThread :: (MonadAction context state m) => m RegisterThread
askRegisterThread = liftAction (Action (\env _trav -> return (aeRegisterThread env)))

{-# INLINE askHandleException #-}
askHandleException :: (MonadAction context state m) => m HandleException
askHandleException = liftAction (Action (\env _trav -> return (aeHandleException env)))

-- Monad
-- --------------------------------------------------------------------

data DomEnv stateRoot = DomEnv
  { domEnvReversePath :: V.Path
  -- ^ this is stored in _reverse_ order. IMPORTANT: updatedomDirtyPath
  -- every time you modify this.
  , domEnvDirtyPath :: Bool
  -- ^ this is whether we have added any path since the last component.
  -- it's used solely to prevent people placing a component on the same
  -- path, which will make them step on each other toes.
  , domEnvComponentName :: Text
  -- ^ used for debugging
  }

newtype DomM dom context state a = DomM
  { unDomM ::
         forall rootState compProps compContext compState.
         ActionEnv rootState
      -> ActionTraverse rootState compProps compContext compState context state
      -> DomEnv rootState
      -> Maybe context
      -> state
      -> dom
      -> DOM.JSM a
  }

type DomState = V.NormalChildren

type KeyedDomState = V.KeyedChildren

type Dom ctx st = DomM DomState ctx st ()
type Dom' ctx st a = DomM DomState ctx st a
type KeyedDom ctx st = DomM KeyedDomState ctx st ()
type KeyedDom' ctx st a = DomM KeyedDomState ctx st a
type Node ctx st = DomM () ctx st V.Node
type Node' ctx st a = DomM () ctx st (V.Node, a)

instance MonadIO (DomM dom ctx st) where
  {-# INLINE liftIO #-}
  liftIO m = DomM $ \_actEnv _acTrav _anEnv _ctx _st _dom -> do
    x <- liftIO m
    return x

#if !defined(ghcjs_HOST_OS)
instance JS.MonadJSM (DomM dom ctx st) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = DomM (\_actEnv _acTrav _anEnv _ctx _st _dom -> m)
#endif

instance Functor (DomM dom ctx st) where
  {-# INLINE fmap #-}
  fmap f (DomM g) = DomM $ \acEnv acTrav aeEnv ctx st dom -> do
    x <- g acEnv acTrav aeEnv ctx st dom
    return (f x)

instance Applicative (DomM dom ctx st) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (DomM dom ctx st) where
  {-# INLINE return #-}
  return x = DomM (\_acEnv _acTrav _aeEnv _ctx _st _dom -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = DomM $ \acEnv acTrav anEnv ctx st dom -> do
    x <- unDomM ma acEnv acTrav anEnv ctx st dom
    y <- unDomM (mf x) acEnv acTrav anEnv ctx st dom
    return y

instance (a ~ ()) => Monoid (DomM dom ctx st a) where
  {-# INLINE mempty #-}
  mempty = return ()
  {-# INLINE mappend #-}
  mappend = (>>)

instance MonadAction ctx st (DomM dom ctx st) where
  {-# INLINE liftAction #-}
  liftAction (Action f) = DomM $ \acEnv acTrav _anEnv _ctx _st _dom -> do
    x <- f acEnv acTrav
    return x

instance MonadReader st (DomM dom ctx st) where
  {-# INLINE ask #-}
  ask = DomM (\_acEnv _acTrav _anEnv _ctx st _dom -> return st)
  {-# INLINE local #-}
  local f m = DomM $ \acEnv acTrav anEnv ctx st dom -> do
    unDomM m acEnv acTrav anEnv ctx (f st) dom

assertContext :: HasCallStack => Text -> Maybe ctx -> ctx
assertContext compName mbCtx =
  case mbCtx of
    Nothing -> do
      error ("Could not get the component for viewer " <> T.unpack compName)
    Just ctx -> ctx

{-# INLINE askContext #-}
askContext :: HasCallStack => DomM dom ctx st ctx
askContext = DomM $ \_acEnv _acTrav anEnv mbCtx _st _dom ->
  return (assertContext (domEnvComponentName anEnv) mbCtx)

{-# INLINE viewContext #-}
viewContext :: HasCallStack => Lens.Getting a ctx a -> DomM dom ctx st a
viewContext l = DomM $ \_acEnv _acTrav anEnv mbCtx _st _dom ->
  return (view l (assertContext (domEnvComponentName anEnv) mbCtx))

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
    (Just ctx)
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
    (fmap (view l) ctx)
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
  ix <- liftIO (V.normalChildrenLength dom)
  nod <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = V.PathNormal ix : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  liftIO (V.pushNormalChild dom nod)

{-# INLINE n' #-}
n' :: Node' ctx st a -> Dom' ctx st a
n' getNode = DomM $ \acEnv acTrav anEnv ctx st dom -> do
  ix <- liftIO (V.normalChildrenLength dom)
  (nod, x) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = V.PathNormal ix : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  liftIO (V.pushNormalChild dom nod)
  return x

{-# INLINE key #-}
key :: Text -> Node ctx st -> KeyedDom ctx st
key k getNode = DomM $ \acEnv acTrav anEnv ctx st dom -> do
  nod <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = V.PathKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  liftIO (V.pushKeyedChild dom k nod)

{-# INLINE key' #-}
key' :: Text -> Node' ctx st a -> KeyedDom' ctx st a
key' k getNode = DomM $ \acEnv acTrav anEnv ctx st dom -> do
  (nod, x) <- unDomM
    getNode
    acEnv
    acTrav
    anEnv
      { domEnvReversePath = V.PathKeyed k : domEnvReversePath anEnv
      , domEnvDirtyPath = True
      }
    ctx
    st
    ()
  liftIO (V.pushKeyedChild dom k nod)
  return x

{-# INLINE text #-}
text :: Text -> Node ctx st
text txt = liftIO (V.node (V.NBText txt))

instance (el ~ DOM.Text) => IsString (Node ctx st) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE rawNode #-}
rawNode ::
     DOM.IsNode el
  => el
  -> [V.AddCallback]
  -> Node ctx st
rawNode x patches = do
  node <- liftIO (V.node (V.NBRaw (DOM.toNode x)))
  return (foldl' V.addNodeCallback node patches)

{-# INLINABLE marked #-}
marked :: StaticPtr (Node context state) -> Node context state
marked ptr = DomM $ \acEnv acTrav domEnv ctx st dom -> do
  let !fprint = staticKey ptr
  node <- unDomM (deRefStaticPtr ptr) acEnv acTrav domEnv ctx st dom
  return (V.setNodeMark node (Just (T.pack (show fprint))))

-- Utilities to quickly create nodes
-- --------------------------------------------------------------------

data SomeEventAction el ctx st = forall e. (DOM.IsEvent e) =>
  SomeEventAction Text (el -> e -> Action ctx st ())
newtype UnsafeRawHtml = UnsafeRawHtml Text

data NodePatch el ctx st =
    NPWillMount (el -> Action ctx st ())
  | NPDidMount (el -> Action ctx st ())
  | NPWillPatch (el -> Action ctx st ())
  | NPDidPatch (el -> Action ctx st ())
  | NPWillRemove (el -> Action ctx st ())
  | NPStyle Text Text
  | NPRawAttribute Text (DOM.JSM DOM.JSVal)
  | NPTextAttribute Text Text
  | NPBoolAttribute Text Bool
  | NPTextProperty Text Text
  | NPBoolProperty Text Bool
  | NPRawProperty Text (DOM.JSM DOM.JSVal)
  | NPEvent (SomeEventAction el ctx st)
  | NPClasses [Text]

class IsElementChildren a ctx st where
  elementChildren :: HasCallStack => a -> DomM () ctx st V.Children
instance IsElementChildren () ctx st where
  {-# INLINE elementChildren #-}
  elementChildren _ = do
    dom <- liftIO V.normalChildren
    return (V.ChildrenNormal dom)
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM DomState ctx1 st1 a) ctx2 st2 where
  {-# INLINE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    dom <- liftIO V.normalChildren
    void (f acEnv acTrav anEnv ctx st dom)
    return (V.ChildrenNormal dom)
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM KeyedDomState ctx1 st1 a) ctx2 st2 where
  {-# INLINE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    dom <- liftIO V.keyedChildren
    void (f acEnv acTrav anEnv ctx st dom)
    return (V.ChildrenKeyed dom)
{-
instance (a ~ (), ctx1 ~ ctx2, st1 ~ st2) => IsElementChildren (DomM MapDomState ctx1 st1 a) ctx2 st2 where
  {-# INLINABLE elementChildren #-}
  elementChildren (DomM f) = DomM $ \acEnv acTrav anEnv ctx st _ -> do
    (MapDomState dom, _) <- f acEnv acTrav anEnv ctx st (MapDomState mempty)
    let kvs = DList.toList dom
    let numKvs = length kvs
    let hm = HMS.fromList kvs
    if numKvs /= HMS.size hm
      then error "duplicate keys when building map vdom!"
      else return ((), V.CMap (HMS.fromList kvs))
-}
instance IsElementChildren UnsafeRawHtml ctx st where
  {-# INLINE elementChildren #-}
  elementChildren (UnsafeRawHtml txt) = return (V.ChildrenRawHtml txt)

-- if the node doesn't contain an element, this functin will crash
patchElement :: forall el ctx st.
     ( DOM.IsElement el )
  => V.Node
  -> [NodePatch el ctx st]
  -> Node ctx st
patchElement node0 patches0 = liftAction $ do
  u <- askUnliftJSM
  let
    wrapCallback :: (el -> Action ctx st ()) -> (DOM.HTMLElement -> DOM.JSM ())
    wrapCallback cback (DOM.HTMLElement el_) = unliftJSM u . cback =<< DOM.fromJSValUnchecked el_
  let
    mkPatches :: V.Node -> [NodePatch el ctx st] -> JS.JSM V.Node
    mkPatches !node = \case
      [] -> return node
      patch : patches -> case patch of
        NPWillMount cback -> mkPatches
          (V.addNodeCallback node (V.ACWillMount (wrapCallback cback)))
          patches
        NPDidMount cback -> mkPatches
          (V.addNodeCallback node (V.ACDidMount (wrapCallback cback)))
          patches
        NPWillPatch cback -> mkPatches
          (V.addNodeCallback node (V.ACWillPatch (wrapCallback cback)))
         patches
        NPDidPatch cback -> mkPatches
          (V.addNodeCallback node (V.ACDidPatch (wrapCallback cback)))
          patches
        NPWillRemove cback -> mkPatches
          (V.addNodeCallback node (V.ACWillRemove (wrapCallback cback)))
          patches
        NPStyle styleName styleBody -> do
          liftIO (V.patchElement node (V.EPStyle styleName styleBody))
          mkPatches node patches
        NPRawAttribute attrName attrBody -> do
          attrBodyVal <- attrBody
          liftIO (V.patchElement node (V.EPRawAttribute attrName attrBodyVal))
          mkPatches node patches
        NPTextAttribute attrName attrBody -> do
          liftIO (V.patchElement node (V.EPTextAttribute attrName attrBody))
          mkPatches node patches
        NPBoolAttribute attrName attrBody -> do
          liftIO (V.patchElement node (V.EPBoolAttribute attrName attrBody))
          mkPatches node patches
        NPRawProperty propName propBody -> do
          propBodyVal <- DOM.liftJSM propBody
          liftIO (V.patchElement node (V.EPRawProperty propName propBodyVal))
          mkPatches node patches
        NPTextProperty attrName attrBody -> do
          liftIO (V.patchElement node (V.EPTextProperty attrName attrBody))
          mkPatches node patches
        NPBoolProperty attrName attrBody -> do
          liftIO (V.patchElement node (V.EPBoolProperty attrName attrBody))
          mkPatches node patches
        NPEvent (SomeEventAction evName evListener) -> do
          liftIO $ V.patchElement node $ V.EPEvent evName $ \el_ ev_ -> do
            unliftJSM u (evListener (coerce el_) (coerce ev_))
          mkPatches node patches
        NPClasses classes -> do
          liftIO $ for_ classes $ \class_ ->
            V.patchElement node (V.EPClass class_)
          mkPatches node patches
  DOM.liftJSM (mkPatches node0 patches0)

{-# INLINABLE el #-}
el :: forall a ctx st el.
     ( IsElementChildren a ctx st
     , DOM.IsElement el
     , DOM.ToJSVal el
     , HasCallStack
     )
  => Text
  -> [NodePatch el ctx st]
  -> a
  -> Node ctx st
el tag patches0 isChildren = do
  children <- elementChildren isChildren
  vel <- liftIO (V.element tag children)
  node0 <- liftIO (V.node (V.NBElement vel))
  patchElement node0 patches0

-- Properties
-- --------------------------------------------------------------------

{-# INLINE textProperty #-}
textProperty :: Text -> Text -> NodePatch el context state
textProperty = NPTextProperty

{-# INLINE boolProperty #-}
boolProperty :: Text -> Bool -> NodePatch el context state
boolProperty = NPBoolProperty

{-# INLINE rawProperty #-}
rawProperty :: DOM.ToJSVal a => Text -> a -> NodePatch el context state
rawProperty k v = NPRawProperty k (DOM.toJSVal v)

{-# INLINE style #-}
style :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> NodePatch el context state
style = NPStyle

{-# INLINE textAttribute #-}
-- | Note: for all standard HTML attributes (e.g. class, src, etc.), you
-- should use @property@, not @attribute@.
textAttribute :: Text -> Text -> NodePatch el context state
textAttribute = NPTextAttribute

{-# INLINE boolAttribute #-}
-- | Note: for all standard HTML attributes (e.g. class, src, etc.), you
-- should use @property@, not @attribute@.
boolAttribute :: Text -> Bool -> NodePatch el context state
boolAttribute = NPBoolAttribute

{-# INLINE rawAttribute #-}
-- | Note: for all standard HTML attributes (e.g. class, src, etc.), you
-- should use @property@, not @attribute@.
rawAttribute :: DOM.ToJSVal a => Text -> a -> NodePatch el context state
rawAttribute k v = NPRawAttribute k (DOM.toJSVal v)

{-
{-# INLINE onEvent #-}
onEvent ::
     (DOM.IsEventTarget t, DOM.IsEvent e, DOM.MonadJSM m, MonadUnliftIO m)
  => t -> DOM.EventM.EventName t e -> (e -> m ()) -> m (DOM.JSM ())
onEvent el_ evName f = do
  u <- askUnliftIO
  DOM.liftJSM $ DOM.EventM.on el_ evName $ do
    ev <- ask
    liftIO (unliftIO u (f ev))
-}

-- simple rendering
-- --------------------------------------------------------------------

{-# INLINABLE simpleNode #-}
simpleNode :: forall state. state -> Node () state -> DOM.JSM V.Node
simpleNode st node0 = do
  comp <- newComponent st (\() -> node0)
  liftIO (writeIORef (_componentContext comp) (Just ()))
  vdom <- unDomM
    (do
      node <- _componentNode comp ()
      patches <- registerComponent (_componentName comp) (_componentPositions comp) ()
      return (foldl' V.addNodeCallback node patches))
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
      , domEnvComponentName = _componentName comp
      }
    (Just ())
    st
    ()
  return vdom

-- when we want a quick render of a component, e.g. inside a raw node.
-- any attempt to use dispatch will result in an exception; e.g. this
-- will never redraw anything, it's just to quickly draw some elements
{-# INLINABLE simpleRenderNode #-}
simpleRenderNode :: state -> Node () state -> DOM.JSM DOM.Node
simpleRenderNode st node = do
  vdom <- simpleNode st node
  V.renderedNodeDom =<< V.render vdom (\_ -> return ())

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
  , _componentPositions :: IORef (HMS.HashMap V.Path props)
  , _componentContext :: IORef (Maybe ctx)
  , _componentFingerprint :: Maybe Text
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

{-# INLINABLE newNamedComponent #-}
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
{-# INLINABLE newMarkedComponent #-}
newMarkedComponent ::
     MonadIO m
  => Text
  -> state
  -> StaticPtr (props -> Node context state)
  -> m (Component props context state)
newMarkedComponent name st node = do
  posRef <- liftIO (newIORef mempty)
  ctxRef <- liftIO (newIORef Nothing)
  return (Component st (deRefStaticPtr node) posRef ctxRef (Just (T.pack (show (staticKey node)))) name)

{-# INLINABLE newMarkedComponent_ #-}
newMarkedComponent_ ::
     MonadIO m
  => Text
  -> state
  -> StaticPtr (Node context state)
  -> m (Component () context state)
newMarkedComponent_ name st node = do
  posRef <- liftIO (newIORef mempty)
  ctxRef <- liftIO (newIORef Nothing)
  return (Component st (\() -> deRefStaticPtr node) posRef ctxRef (Just (T.pack (show (staticKey node)))) name)

{-# INLINABLE newComponent #-}
newComponent ::
     MonadIO m
  => state
  -> (props -> Node context state)
  -> m (Component props context state)
newComponent = newNamedComponent "<no-name>"

{-# INLINABLE newNamedComponent_ #-}
newNamedComponent_ ::
     MonadIO m
  => Text
  -> st
  -> Node ctx st
  -> m (Component () ctx st)
newNamedComponent_ name st comp = newNamedComponent name st (\() -> comp)

{-# INLINABLE newComponent_ #-}
newComponent_ ::
     MonadIO m
  => st
  -> Node ctx st
  -> m (Component () ctx st)
newComponent_ st comp = newComponent st (\() -> comp)

{-# INLINABLE registerComponent #-}
registerComponent :: Text -> IORef (HMS.HashMap V.Path props) -> props -> DomM dom a b [V.AddCallback]
registerComponent name ref props = DomM $ \_acEnv _acTrav anEnv _ctx _st _dom -> do
  let add txt = \_ -> do
        logDebug (txt <> ": adding component " <> name)
        liftIO (modifyIORef' ref (HMS.insert (reverse (domEnvReversePath anEnv)) props))
  let remove txt = \_ -> do
        logDebug (txt <> ": removing component " <> name)
        liftIO (modifyIORef' ref (HMS.delete (reverse (domEnvReversePath anEnv))))
  return
    [ V.ACWillMount (add "willMount")
    , V.ACWillPatch (remove "willPatch")
    , V.ACDidPatch (add "didPatch")
    , V.ACWillRemove (remove "willRemove")
    ]

{-# INLINE initComponent #-}
initComponent :: Component props ctx st -> ctx -> DomM dom ctx0 st0 (ComponentToken props ctx st)
initComponent comp ctx = liftIO $ do
  writeIORef (_componentContext comp) (Just ctx)
  return (ComponentToken (_componentContext comp))

{-# INLINABLE component #-}
component :: props -> ComponentToken props ctx st -> Node ctx0 (Component props ctx st)
component props (ComponentToken tok) = do
  (node, name, pos, mbFprint) <- DomM $ \acEnv acTrav anEnv _ctx comp dom -> do
    let name = _componentName comp
    when (_componentContext comp /= tok) $
      error (T.unpack name <> ": Initialized component does not match state component!")
    unless (domEnvDirtyPath anEnv) $
      error (T.unpack name <> ": Trying to insert component immediately inside another component at path " <> show (reverse (domEnvReversePath anEnv)) <> ", please wrap the inner component in a node.")
    mbCtx <- liftIO (readIORef (_componentContext comp))
    node <- unDomM
      (_componentNode comp props)
      acEnv
      acTrav
        { atToComp = atToComp acTrav . compState . atToState acTrav
        , atToState = id
        , atToContext = id
        }
      anEnv
        { domEnvDirtyPath = False
        , domEnvComponentName = _componentName comp
        }
      mbCtx
      (_componentState comp)
      dom
    return (node, name, _componentPositions comp, _componentFingerprint comp)
  patches <- registerComponent name pos props
  -- keep in sync with similar code in Anapo.Loop.nodeLoop.runComp
  let node' = foldl' V.addNodeCallback node patches
  return $ case mbFprint of
    Nothing -> node'
    Just fprint -> V.setNodeMark node' (Just fprint)

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
