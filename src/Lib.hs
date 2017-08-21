{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib (run) where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, over, makeLenses, at, _Just)
import Control.Concurrent.Chan (Chan, writeChan, readChan, newChan)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Control.Monad (forM_, when, unless)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.String (IsString(..))
import Control.Exception (BlockedIndefinitelyOnMVar, try)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map.Strict as Map
import Data.List (partition)
import Control.Monad.Trans.Class (lift)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

-- How to run the client monad
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)
runClientM :: ClientM () -> IO ()
runClientM = id
#else
import qualified Language.Javascript.JSaddle.Warp as DOM
runClientM :: ClientM () -> IO ()
runClientM cm = do
  putStrLn "Running on port 8000"
  DOM.run 8000 cm
#endif

-- Components and dispatching
-- --------------------------------------------------------------------

type ClientM = DOM.JSM

type Dispatch state = (state -> state) -> ClientM ()

newtype Component state = Component {componentRender :: Dispatch state -> state -> VirtualDom}

componentFocus :: Lens' out in_ -> Component in_ -> Component out
componentFocus l (Component c) = Component $ \disp st ->
  c (\modState -> disp (over l modState)) (view l st)

insertComponent :: Lens' out in_ -> Component in_ -> Dispatch out -> out -> VirtualDom
insertComponent l comp = componentRender (componentFocus l comp)

type VirtualDom = [VirtualDomNode]

data VirtualDomNode =
    VDElement VirtualDomElement
  | VDText T.Text

data SomeEvent = forall e. (DOM.IsEvent e) =>
  SomeEvent (forall t. (DOM.IsEventTarget t, DOM.IsGlobalEventHandlers t) => (DOM.EventName t e, DOM.EventM t e ()))

data VirtualDomElement = VirtualDomElement
  { vdeTag :: T.Text
  , vdeAttributes :: HMS.HashMap T.Text T.Text
  , vdeEvents :: [SomeEvent]
  , vdeChildren :: VirtualDom
  }

componentLoop :: forall state. state -> Component state -> (VirtualDom -> ClientM ()) -> ClientM ()
componentLoop !st0 (Component render) useDom = do
  toDispatch :: Chan (state -> state) <- liftIO newChan
  let
    go !st = do
      useDom (render (liftIO . writeChan toDispatch) st)
      mbF <- liftIO $ do
        mbF :: Either BlockedIndefinitelyOnMVar (state -> state) <- try (readChan toDispatch)
        case mbF of
          Left{} -> do
            putStrLn "BLOCKED FOREVER ON MVAR, TERMINATING COMPONENT LOOP"
            return Nothing
          Right x -> return (Just x)
      forM_ mbF (\f -> go (f st))
  go st0

-- Virtual dom to HTML
-- --------------------------------------------------------------------

renderVirtualDom :: (DOM.IsNode el) => DOM.Document -> el -> VirtualDom -> ClientM ()
renderVirtualDom doc container0 vd0 = do
  -- remove all children
  let removeAllChildren = do
        mbChild <- DOM.getFirstChild container0
        forM_ mbChild $ \child -> do
          DOM.removeChild_ container0 child
          removeAllChildren
  removeAllChildren
  -- now insert new vdom
  let
    go :: (DOM.IsNode el) => el -> VirtualDomNode -> ClientM ()
    go container = \case
      VDText txt -> do
        txtNode <- DOM.createTextNode doc txt
        DOM.appendChild_ container txtNode
      VDElement VirtualDomElement{..} -> do
        el_ <- DOM.uncheckedCastTo DOM.HTMLElement <$> DOM.createElement doc vdeTag
        forM_ (HMS.toList vdeAttributes) (uncurry (DOM.setAttribute el_))
        forM_ vdeEvents (\(SomeEvent (name, ev)) -> DOM.on el_ name ev)
        forM_ vdeChildren (go el_)
        DOM.appendChild_ container el_
  mapM_ (go container0) vd0

installComponent :: state -> Component state -> ClientM ()
installComponent st comp = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  componentLoop st comp $ \vd -> do
    renderVirtualDom doc body vd
    DOM.syncPoint -- force jssaddle stuff

-- Monadic interface to VDom
-- --------------------------------------------------------------------

newtype VirtualDomM a = VirtualDomM (Seq VirtualDomNode, a)
  deriving (Functor, Applicative, Monad)

runVirtualDomM :: VirtualDomM () -> VirtualDom
runVirtualDomM (VirtualDomM (nodes, ())) = toList nodes

instance (a ~ ()) => IsString (VirtualDomM a) where
  fromString = text . T.pack

el :: T.Text -> [(T.Text, T.Text)] -> [SomeEvent] -> VirtualDomM () -> VirtualDomM ()
el tag attrs evs vdomm = let
  vdel = VirtualDomElement
    { vdeTag = tag
    , vdeAttributes = HMS.fromList attrs
    , vdeEvents = evs
    , vdeChildren = runVirtualDomM vdomm
    }
  in VirtualDomM (Seq.singleton (VDElement vdel), ())

text :: T.Text -> VirtualDomM ()
text txt = VirtualDomM (Seq.singleton (VDText txt), ())

component :: Lens' out in_ -> Component in_ -> Dispatch out -> out -> VirtualDomM ()
component l comp disp st = do
  let vdom = componentRender (componentFocus l comp) disp st
  VirtualDomM (Seq.fromList vdom, ())

-- TestApp
-- --------------------------------------------------------------------

data TodoItemState = TodoItemState
  { _tisCompleted :: Bool
  , _tisBody :: T.Text
  } deriving (Eq, Show)
makeLenses ''TodoItemState

data TodoState = TodoState
  { _tsShowCompleted :: Bool
  , _tsTodoElements :: Map.Map Int TodoItemState
  , _tsCurrentText :: Text
  } deriving (Eq, Show)
makeLenses ''TodoState

todoItemComponent :: Component (Maybe TodoItemState)
todoItemComponent = Component $ \dispatch -> \case
  Nothing -> error "No state for todo item!"
  Just st -> runVirtualDomM $ do
    unless (_tisCompleted st)  $
      el "input"
        [("type", "checkbox")]
        [ SomeEvent
            ( DOM.change
            , lift (dispatch (over (_Just . tisCompleted) not))
            )
        ]
        (return ())
    text (_tisBody st)

todoComponent :: Component TodoState
todoComponent = Component $ \dispatch st -> runVirtualDomM $ do
  el "a"
    [ ("href", "#") ]
    [ SomeEvent
        ( DOM.click
        , DOM.preventDefault >> lift (dispatch (over tsShowCompleted not))
        )
    ] $ if _tsShowCompleted st then "Hide completed tasks" else "Show completed tasks"
  el "li" [] [] $ do
    let (done, active) = partition (_tisCompleted . snd) (Map.toAscList (_tsTodoElements st))
    let renderItems items =
          el "ul" [] [] $
            forM_ items $ \(key, _item) -> el "li" [] [] $
              component (tsTodoElements . at key) todoItemComponent dispatch st
    renderItems active
    when (_tsShowCompleted st) $ do
      el "h2" [] [] "Completed"
      renderItems done

run :: IO ()
run = runClientM $ installComponent
  TodoState
    { _tsShowCompleted = True
    , _tsTodoElements = Map.fromList
        [ (1, TodoItemState{_tisCompleted = True, _tisBody = "Buy milk"})
        , (2, TodoItemState{_tisCompleted = False, _tisBody = "Get a life"})
        ]
    }
  todoComponent
