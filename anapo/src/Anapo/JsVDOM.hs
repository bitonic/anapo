-- TODO replace all the JS.function with synchronous versions in ghcjs
{-# LANGUAGE CPP #-}
module Anapo.JsVDOM
  ( Node
  , NodeBody(..)
  , node
  , setNodeMark
  , addNodeCallback
  , AddCallback(..)

  , NormalChildren
  , normalChildren
  , pushNormalChild
  , normalChildrenLength
  , KeyedChildren
  , keyedChildren
  , pushKeyedChild

  , element
  , patchElement
  , ElementPatch(..)
  , Children(..)

  , PathSegment(..)
  , Path
  , RenderedNode
  , render
  , reconciliate
  , renderedNodeDom
  ) where

import Language.Javascript.JSaddle (JSVal, JSM)
import qualified Language.Javascript.JSaddle as JS
import qualified GHCJS.DOM.Types as DOM
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Hashable (Hashable(..))
import Control.Exception.Safe (bracket)
import GHC.Prim (coerce)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Anapo.Text (pack, Text)
import Anapo.Logging

#if defined(ghcjs_HOST_OS)
import qualified GHCJS.Foreign.Callback as GHCJS
import qualified GHCJS.Foreign.Callback.Internal as GHCJS
#endif

data Node = Node
  { nodeBody :: NodeBody
  , nodeMark :: Maybe Text
  , nodeCallbacks :: Maybe Callbacks
  }

data NodeBody =
    NBElement Element
  | NBText Text
  | NBRaw DOM.Node

{-# INLINE node #-}
node ::
     NodeBody
  -> JSM Node
node nodeBody = return Node{nodeBody, nodeCallbacks = Nothing, nodeMark = Nothing}

data Callbacks = Callbacks
  { callbacksWillMount :: Maybe (DOM.HTMLElement -> JSM ())
  , callbacksDidMount :: Maybe (DOM.HTMLElement -> JSM ())
  , callbacksWillPatch :: Maybe (DOM.HTMLElement -> JSM ())
  , callbacksDidPatch :: Maybe (DOM.HTMLElement -> JSM ())
  , callbacksWillRemove :: Maybe (DOM.HTMLElement -> JSM ())
  }

data AddCallback =
    ACWillMount (DOM.HTMLElement -> JSM ())
  | ACDidMount (DOM.HTMLElement -> JSM ())
  | ACWillPatch (DOM.HTMLElement -> JSM ())
  | ACDidPatch (DOM.HTMLElement -> JSM ())
  | ACWillRemove (DOM.HTMLElement -> JSM ())

addNodeCallback ::
     Node
  -> AddCallback
  -> Node
addNodeCallback nod@Node{nodeCallbacks} addCback =
  let nodeCallbacks' = case addCback of
        ACWillMount cback ->
          Just $ case nodeCallbacks of
            Nothing -> emptyCallbacks{ callbacksWillMount = Just cback }
            Just callbacks -> case callbacksWillMount callbacks of
              Nothing -> callbacks{ callbacksWillMount = Just cback }
              Just willMount -> callbacks{ callbacksWillMount = Just (\el_ -> willMount el_ >> cback el_) }
        ACDidMount cback ->
          Just $ case nodeCallbacks of
            Nothing -> emptyCallbacks{ callbacksDidMount = Just cback }
            Just callbacks -> case callbacksDidMount callbacks of
              Nothing -> callbacks{ callbacksDidMount = Just cback }
              Just didMount -> callbacks{ callbacksDidMount = Just (\el_ -> didMount el_ >> cback el_) }
        ACWillPatch cback ->
          Just $ case nodeCallbacks of
            Nothing -> emptyCallbacks{ callbacksWillPatch = Just cback }
            Just callbacks -> case callbacksWillPatch callbacks of
              Nothing -> callbacks{ callbacksWillPatch = Just cback }
              Just willPatch -> callbacks{ callbacksWillPatch = Just (\el_ -> willPatch el_ >> cback el_) }
        ACDidPatch cback ->
          Just $ case nodeCallbacks of
            Nothing -> emptyCallbacks{ callbacksDidPatch = Just cback }
            Just callbacks -> case callbacksDidPatch callbacks of
              Nothing -> callbacks{ callbacksDidPatch = Just cback }
              Just didPatch -> callbacks{ callbacksDidPatch = Just (\el_ -> didPatch el_ >> cback el_) }
        ACWillRemove cback ->
          Just $ case nodeCallbacks of
            Nothing -> emptyCallbacks{ callbacksWillRemove = Just cback }
            Just callbacks -> case callbacksWillRemove callbacks of
              Nothing -> callbacks{ callbacksWillRemove = Just cback }
              Just willRemove -> callbacks{ callbacksWillRemove = Just (\el_ -> willRemove el_ >> cback el_) }
  in nod{nodeCallbacks = nodeCallbacks'}
  where
    emptyCallbacks = Callbacks Nothing Nothing Nothing Nothing Nothing

finalizeNode :: Node -> JSM JSVal
finalizeNode Node{..} = do
  nodeObj <- JS.obj
  case nodeBody of
    NBRaw el -> (nodeObj JS.<# "raw") el
    NBElement (Element el) -> (nodeObj JS.<# "element") el
    NBText txt -> (nodeObj JS.<# "text") txt
  for_ nodeCallbacks $ \Callbacks{..} -> do
#if defined(ghcjs_HOST_OS)
    cbacksObj <- js_emptyArray
#else
    cbacksObj <- JS.obj
#endif
    for_ callbacksWillMount $ \willMount -> do
      storeCallback cbacksObj willMount "willMount"
    for_ callbacksDidMount $ \didMount ->
      storeCallback cbacksObj didMount "didMount"
    for_ callbacksWillPatch $ \willPatch ->
      storeCallback cbacksObj willPatch "willPatch"
    for_ callbacksDidPatch $ \didPatch ->
      storeCallback cbacksObj didPatch "didPatch"
    for_ callbacksWillRemove $ \willRemove ->
      storeCallback cbacksObj willRemove "willRemove"
    (nodeObj JS.<# "callbacks") cbacksObj
  for_ nodeMark (\mark -> (nodeObj JS.<# "mark") mark)
  JS.toJSVal nodeObj
  where
    storeCallback cbacksObj fun label = do
#if defined(ghcjs_HOST_OS)
      funVal <- GHCJS.syncCallback1 GHCJS.ThrowWouldBlock $ \el -> do
        fun =<< DOM.unsafeCastTo DOM.HTMLElement el
      funObj <- js_newLifecycleCallback funVal funVal
      js_setProperty cbacksObj (pack label) funObj
#else
      funFun <- JS.function $ \_ _ [el] -> do
        fun =<< DOM.unsafeCastTo DOM.HTMLElement el
      funObj <- JS.jsg"Anapo" ^. JS.jsf"newLifecycleCallback" (funFun, funFun)
      (cbacksObj JS.<# label) funObj
#endif

setNodeMark ::
     Node
  -> Maybe Text
  -> Node
setNodeMark nod mbMark = nod{nodeMark = mbMark}

newtype Element = Element JSVal

{-# INLINE element #-}
element ::
     Text -- ^ tag
  -> Children
  -> JSM Element
element tag children = do
#if defined(ghcjs_HOST_OS)
  elObj <- js_newElement tag
  case children of
    ChildrenRawHtml txt ->
      js_setRawHtml elObj txt
    ChildrenNormal (NormalChildren nodes) -> do
      js_setNormal elObj nodes
    ChildrenKeyed (KeyedChildren kvs) -> do
      js_setKeyed elObj kvs
  return (Element elObj)
#else
  elObj <- JS.jsg "Anapo" ^. JS.jsf"newElement" [tag]
  childrenObj <- JS.obj
  case children of
    ChildrenRawHtml txt ->
      (childrenObj JS.<# "rawHtml") txt
    ChildrenNormal (NormalChildren nodes) -> do
      (childrenObj JS.<# "normal") nodes
    ChildrenKeyed (KeyedChildren kvs) -> do
      (childrenObj JS.<# "keyed") kvs
  (elObj JS.<# "children") childrenObj
  Element <$> JS.toJSVal elObj
#endif

-- crashes if we do not have an element inside.
{-# INLINE patchElement #-}
patchElement :: Node -> ElementPatch -> JSM ()
patchElement Node{nodeBody} elPatch = case nodeBody of
#if defined(ghcjs_HOST_OS)
  NBElement (Element el) -> case elPatch of
    EPStyle k v -> js_setStyle el k v
    EPAttribute k v -> js_setAttribute el k v
    EPProperty k v -> js_setElementProperty el k v
    EPClass cls -> js_setClass el cls
    EPEvent type_ evt -> do
      -- TODO replace with direct sync callback in ghcjs
      evtFun <- JS.function $ \_ this [ev] -> do
        this_ <- DOM.unsafeCastTo DOM.HTMLElement this
        ev_ <- DOM.unsafeCastTo DOM.Event ev
        evt this_ ev_
      evtFunVal <- JS.toJSVal evtFun
      js_pushEvent el type_ evtFunVal (coerce (JS.functionCallback evtFun))
#else
  NBElement (Element el) -> case elPatch of
    EPStyle k v -> ((el ^. JS.js "style") JS.<# k) v
    EPAttribute k v -> ((el ^. JS.js "attributes") JS.<# k) v
    EPProperty k v -> ((el ^. JS.js "properties") JS.<# k) v
    EPClass cls -> ((el ^. JS.js "classes") JS.<# cls) True
    EPEvent type_ evt -> do
      evtFun <- JS.function $ \_ this [ev] -> do
        this_ <- DOM.unsafeCastTo DOM.HTMLElement this
        ev_ <- DOM.unsafeCastTo DOM.Event ev
        evt this_ ev_
      evtObj <- JS.jsg"Anapo" ^. JS.jsf"newEventCallback" (type_, evtFun, evtFun)
      void (el ^. JS.js "events" ^. JS.jsf"push" [evtObj])
#endif
  NBRaw{} -> error "got raw in patchElement"
  NBText{} -> error "got raw in patchElement"


data ElementPatch =
    EPStyle Text Text
  | EPAttribute Text JSVal
  | EPProperty Text JSVal
  | EPEvent Text (DOM.HTMLElement -> DOM.Event -> JSM ())
  | EPClass Text

data Children =
    ChildrenRawHtml Text
  | ChildrenNormal NormalChildren
  | ChildrenKeyed KeyedChildren

newtype RenderedNode = RenderedNode JSVal

render :: Node -> (RenderedNode -> JSM ()) -> JSM RenderedNode
render nod cont = do
  t0 <- liftIO getCurrentTime
  nodeVal <- finalizeNode nod
  rvdom <- bracket
    (JS.function $ \_ _ [rendered] -> do
      cont (RenderedNode rendered))
    JS.freeFunction
    (\contFun -> do
        RenderedNode <$> (JS.jsg"Anapo" ^. JS.jsf"render" (nodeVal, contFun)))
  t1 <- liftIO getCurrentTime
  logDebug (pack "Vdom rendered (" <> pack (show (diffUTCTime t1 t0)) <> pack ")")
  return rvdom

type Path = [PathSegment]

data PathSegment =
    PathNormal Int
  | PathKeyed Text
  deriving (Eq, Show)
instance Hashable PathSegment where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (PathNormal i) = salt `hashWithSalt` (0::Int) `hashWithSalt` i
  hashWithSalt salt (PathKeyed i) = salt `hashWithSalt` (1::Int) `hashWithSalt` i

reconciliate :: RenderedNode -> Path -> Node -> JSM ()
reconciliate (RenderedNode rvdom) path nod = do
  t0 <- liftIO getCurrentTime
  nodeVal <- finalizeNode nod
  pathVal <- JS.toJSVal =<< (for path $ \case
    PathNormal n -> JS.toJSVal n
    PathKeyed k -> JS.toJSVal k)
  let freeCallback val = do
#if defined(ghcjs_HOST_OS)
        let callback :: GHCJS.Callback (JSVal -> JSVal -> IO ()) = coerce val
        GHCJS.releaseCallback callback
#else
        JS.freeFunction (coerce val)
#endif
  Just (callbacksTokens :: [JS.JSVal]) <- JS.fromJSVal =<< JS.jsg"Anapo" ^. JS.jsf"reconciliate" (rvdom, pathVal, nodeVal)
  for_ callbacksTokens freeCallback
  t1 <- liftIO getCurrentTime
  logDebug (pack "Vdom reconciled (" <> pack (show (diffUTCTime t1 t0)) <> pack ")")

{-# INLINE renderedNodeDom #-}
renderedNodeDom :: RenderedNode -> JSM DOM.Node
renderedNodeDom (RenderedNode rvdom) = coerce <$> rvdom ^. JS.js "dom"

newtype NormalChildren = NormalChildren JSVal

{-# INLINE normalChildren #-}
normalChildren :: JSM NormalChildren
normalChildren = do
#if defined(ghcjs_HOST_OS)
  NormalChildren <$> js_emptyArray
#else
  arr <- JS.array ([] :: [JSVal])
  NormalChildren <$> JS.toJSVal arr
#endif

{-# INLINE pushNormalChild #-}
pushNormalChild :: NormalChildren -> Node -> JSM ()
pushNormalChild (NormalChildren nodes) nod = do
  nodeVal <- finalizeNode nod
#if defined(ghcjs_HOST_OS)
  js_pushNormalChild nodes nodeVal
#else
  void (nodes ^. JS.jsf"push" nodeVal)
#endif

{-# INLINE normalChildrenLength #-}
normalChildrenLength :: NormalChildren -> JSM Int
normalChildrenLength (NormalChildren ns) = do
#if defined(ghcjs_HOST_OS)
  js_arrayLength ns
#else
  Just n <- JS.fromJSVal =<< ns ^. JS.js "length"
  return n
#endif

newtype KeyedChildren = KeyedChildren JSVal

{-# INLINE keyedChildren #-}
keyedChildren :: JSM KeyedChildren
keyedChildren = do
#if defined(ghcjs_HOST_OS)
  KeyedChildren <$> js_keyedChildren
#else
  KeyedChildren <$> (JS.jsg"Anapo" ^. JS.jsf"newKeyedChildren" ())
#endif

{-# INLINE pushKeyedChild #-}
pushKeyedChild :: KeyedChildren -> Text -> Node -> JSM ()
pushKeyedChild (KeyedChildren kvs) k v = do
  vVal <- finalizeNode v
#if defined(ghcjs_HOST_OS)
  js_pushKeyedChild kvs k vVal
#else
  void (kvs ^. JS.js"order" ^. JS.jsf"push" [k])
  ((kvs ^. JS.js"elements") JS.<# k) vVal
#endif

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "window['Anapo']['newLifecycleCallback']($1, $2)"
  js_newLifecycleCallback :: GHCJS.Callback (JSVal -> IO ()) -> GHCJS.Callback (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe
  "$r = {}"
  js_emptyObject :: IO JSVal

foreign import javascript unsafe
  "$r = window['Anapo']['newElement']($1)"
  js_newElement :: Text -> IO JSVal

foreign import javascript unsafe
  "$1['children']['rawHtml'] = $2"
  js_setRawHtml :: JSVal -> Text -> IO ()

foreign import javascript unsafe
  "$1['children']['normal'] = $2"
  js_setNormal :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['children']['keyed'] = $2"
  js_setKeyed :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['style'][$2] = $3"
  js_setStyle :: JSVal -> Text -> Text -> IO ()

foreign import javascript unsafe
  "$1['attributes'][$2] = $3"
  js_setAttribute :: JSVal -> Text -> JSVal -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3"
  js_setProperty :: JSVal -> Text -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['properties'][$2] = $3"
  js_setElementProperty :: JSVal -> Text -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['classes'][$2] = true"
  js_setClass :: JSVal -> Text -> IO ()

foreign import javascript unsafe
  "$1['events'].push(window['Anapo']['newEventCallback']($2, $3, $4))"
  js_pushEvent :: JSVal -> Text -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.push($2)"
  js_pushNormalChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.length"
  js_arrayLength :: JSVal -> IO Int

foreign import javascript unsafe
  "$r = window['Anapo']['newKeyedChildren']()"
  js_keyedChildren :: IO JSVal

foreign import javascript unsafe
  "if ($1['elements'][$2]) { throw 'Duplicate key in keyed children!'; }; $1['order'].push($2); $1['elements'][$2] = $3;"
  js_pushKeyedChild :: JSVal -> Text -> JSVal -> IO ()

foreign import javascript unsafe
  "$r = []"
  js_emptyArray :: IO JSVal

#endif
