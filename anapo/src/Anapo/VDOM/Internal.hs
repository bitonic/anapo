{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Anapo.VDOM.Internal where

import Language.Javascript.JSaddle (JSVal, JSM)
import qualified Language.Javascript.JSaddle as JS
import qualified GHCJS.DOM.Types as DOM
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Lens ((^.))
import Data.Hashable (Hashable(..))
import Control.Exception.Safe (bracket)
import GHC.Prim (coerce)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Anapo.Text (pack, Text)
import Anapo.Logging

#if defined(ghcjs_HOST_OS)
import qualified GHCJS.Foreign.Callback as GHCJS
import qualified GHCJS.Foreign.Callback.Internal as GHCJS
import GHCJS.Marshal.Pure (pToJSVal)
#else
import Control.Monad (void)
import Data.IORef (newIORef, modifyIORef', IORef, readIORef)
import qualified Data.HashMap.Strict as HMS
import Data.Foldable (toList)
#endif

#if !defined(ghcjs_HOST_OS)
data Bwd a =
    B0
  | Bwd a :> a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
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
  -> IO Node
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

#if defined(ghcjs_HOST_OS)
finalizeNode Node{..} = do
  nodeObj <- js_emptyObject
  case nodeBody of
    NBRaw el -> js_setRaw nodeObj (coerce el)
    NBElement (Element el) -> js_setElement nodeObj el
    NBText txt -> js_setText nodeObj txt
  for_ nodeCallbacks $ \Callbacks{..} -> do
    cbacksObj <- js_emptyObject
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
    js_setCallbacks nodeObj cbacksObj
  for_ nodeMark (\mark -> js_setMark nodeObj mark)
  JS.toJSVal nodeObj
  where
    storeCallback cbacksObj fun label = do
      funVal <- GHCJS.syncCallback1 GHCJS.ThrowWouldBlock $ \el -> do
        fun (coerce el)
      funObj <- js_newLifecycleCallback funVal
      js_setProperty cbacksObj (pack label) funObj

#else
finalizeNode Node{..} = do
  nodeObj <- JS.obj
  case nodeBody of
    NBRaw el -> (nodeObj JS.<# "raw") el
    NBElement el -> (nodeObj JS.<# "element") =<< finalizeElement el
    NBText txt -> (nodeObj JS.<# "text") txt
  for_ nodeCallbacks $ \Callbacks{..} -> do
    cbacksObj <- JS.obj
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
      funFun <- JS.function $ \_ _ [el] -> do
        fun (coerce el)
      funObj <- JS.jsg"Anapo" ^. JS.jsf"newLifecycleCallback" (funFun, funFun)
      (cbacksObj JS.<# label) funObj

    finalizeElement :: Element -> JSM JSVal
    finalizeElement Element{..} = do
      elObj <- JS.jsg"Anapo" ^. JS.jsf"newElement" [elementTag]
      childrenObj <- JS.obj
      case elementChildren of
        ChildrenRawHtml txt ->
          (childrenObj JS.<# "rawHtml") txt
        ChildrenNormal children -> do
          (childrenObj JS.<# "normal") =<< finalizeNormalChildren children
        ChildrenKeyed children -> do
          (childrenObj JS.<# "keyed") =<< finalizeKeyedChildren children
      (elObj JS.<# "children") childrenObj
      patches <- liftIO (readIORef elementPatches)
      for_ patches $ \case
        EPStyle k v -> ((elObj ^. JS.js "style") JS.<# k) v
        EPRawAttribute k v -> ((elObj ^. JS.js "attributes") JS.<# k) v
        EPTextAttribute k v -> ((elObj ^. JS.js "attributes") JS.<# k) v
        EPBoolAttribute k v -> ((elObj ^. JS.js "attributes") JS.<# k) v
        EPRawProperty k v -> ((elObj ^. JS.js "properties") JS.<# k) v
        EPTextProperty k v -> ((elObj ^. JS.js "properties") JS.<# k) v
        EPBoolProperty k v -> ((elObj ^. JS.js "properties") JS.<# k) v
        EPClass cls -> ((elObj ^. JS.js "classes") JS.<# cls) True
        EPEvent type_ evt -> do
          evtFun <- JS.function $ \_ this [ev] -> do
            evt (coerce this) (coerce ev)
          evtObj <- JS.jsg"Anapo" ^. JS.jsf"newEventCallback" (type_, evtFun, evtFun)
          void (elObj ^. JS.js "events" ^. JS.jsf"push" [evtObj])
      JS.toJSVal elObj

    finalizeNormalChildren :: NormalChildren -> JSM JSVal
    finalizeNormalChildren NormalChildren{..} = do
      nodes <- mapM finalizeNode . toList =<< liftIO (readIORef normalChildrenNodes)
      JS.toJSVal nodes

    finalizeKeyedChildren :: KeyedChildren -> JSM JSVal
    finalizeKeyedChildren KeyedChildren{..} = do
      kvs <- JS.jsg"Anapo" ^. JS.jsf"newKeyedChildren" ()
      order <- liftIO (readIORef keyedChildrenOrder)
      nodes <- liftIO (readIORef keyedChildrenNodes)
      for_ order $ \k -> do
        val <- finalizeNode (nodes HMS.! k)
        void (kvs ^. JS.js"order" ^. JS.jsf"push" [k])
        ((kvs ^. JS.js"elements") JS.<# k) val
      return kvs


#endif

setNodeMark ::
     Node
  -> Maybe Text
  -> Node
setNodeMark nod mbMark = nod{nodeMark = mbMark}

#if defined(ghcjs_HOST_OS)
newtype Element = Element JSVal
#else
data Element = Element
  { elementTag :: Text
  , elementChildren :: Children
  , elementPatches :: (IORef (Bwd ElementPatch))
  }
#endif

{-# INLINE element #-}
element ::
     Text -- ^ tag
  -> Children
  -> IO Element
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
  patches <- newIORef B0
  return Element
    { elementTag = tag
    , elementChildren = children
    , elementPatches = patches
    }
#endif

-- crashes if we do not have an element inside.
{-# INLINE patchElement #-}
patchElement :: Node -> ElementPatch -> IO ()
patchElement Node{nodeBody} elPatch = case nodeBody of
#if defined(ghcjs_HOST_OS)
  NBElement (Element el) -> case elPatch of
    EPStyle k v -> js_setStyle el k v
    EPTextAttribute k v -> js_setAttribute el k (pToJSVal v)
    EPBoolAttribute k v -> js_setAttribute el k (pToJSVal v)
    EPRawAttribute k v -> js_setAttribute el k v
    EPTextProperty k v -> js_setElementProperty el k (pToJSVal v)
    EPBoolProperty k v -> js_setElementProperty el k (pToJSVal v)
    EPRawProperty k v -> js_setElementProperty el k v
    EPClass cls -> js_setClass el cls
    EPEvent type_ evt -> do
      evtFun <- GHCJS.syncCallback2 GHCJS.ThrowWouldBlock $ \el0 ev -> do
        evt (coerce el0) (coerce ev)
      js_pushEvent el type_ evtFun
#else
  NBElement el -> modifyIORef' (elementPatches el) (:> elPatch)
#endif
  NBRaw{} -> error "got raw in patchElement"
  NBText{} -> error "got raw in patchElement"

data ElementPatch =
    EPStyle Text Text
  | EPTextAttribute Text Text
  | EPBoolAttribute Text Bool
  | EPRawAttribute Text JSVal
  | EPTextProperty Text Text
  | EPBoolProperty Text Bool
  | EPRawProperty Text JSVal
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

#if defined(ghcjs_HOST_OS)
newtype NormalChildren = NormalChildren JSVal
#else
data NormalChildren = NormalChildren
  { normalChildrenSize :: IORef Int
  , normalChildrenNodes :: IORef (Bwd Node)
  }
#endif

{-# INLINE normalChildren #-}
normalChildren :: IO NormalChildren
normalChildren = do
#if defined(ghcjs_HOST_OS)
  NormalChildren <$> js_emptyArray
#else
  NormalChildren <$> newIORef 0 <*> newIORef B0
#endif

{-# INLINE pushNormalChild #-}
pushNormalChild :: NormalChildren -> Node -> IO ()
#if defined(ghcjs_HOST_OS)
pushNormalChild (NormalChildren nodes) nod = do
  nodeVal <- finalizeNode nod
  js_pushNormalChild nodes nodeVal
#else
pushNormalChild NormalChildren{..} nod = do
  modifyIORef' normalChildrenSize (+1)
  modifyIORef' normalChildrenNodes (:> nod)
#endif

{-# INLINE normalChildrenLength #-}
normalChildrenLength :: NormalChildren -> IO Int
#if defined(ghcjs_HOST_OS)
normalChildrenLength (NormalChildren ns) = do
  js_arrayLength ns
#else
normalChildrenLength NormalChildren{..} = do
  readIORef normalChildrenSize
#endif

#if defined(ghcjs_HOST_OS)
newtype KeyedChildren = KeyedChildren JSVal
#else
data KeyedChildren = KeyedChildren
  { keyedChildrenOrder :: IORef (Bwd Text)
  , keyedChildrenNodes :: IORef (HMS.HashMap Text Node)
  }
#endif

{-# INLINE keyedChildren #-}
keyedChildren :: IO KeyedChildren
keyedChildren = do
#if defined(ghcjs_HOST_OS)
  KeyedChildren <$> js_keyedChildren
#else
  KeyedChildren <$> newIORef B0 <*> newIORef HMS.empty
#endif

{-# INLINE pushKeyedChild #-}
pushKeyedChild :: KeyedChildren -> Text -> Node -> IO ()
#if defined(ghcjs_HOST_OS)
pushKeyedChild (KeyedChildren kvs) k v = do
  vVal <- finalizeNode v
  js_pushKeyedChild kvs k vVal
#else
pushKeyedChild KeyedChildren{..} k v = do
  modifyIORef' keyedChildrenOrder (:> k)
  modifyIORef' keyedChildrenNodes (HMS.insert k v)
#endif

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "window['Anapo']['newLifecycleCallback']($1, $1)"
  js_newLifecycleCallback :: GHCJS.Callback (JSVal -> IO ()) -> IO JSVal

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
  "var __wrapped = function(ev) { $3(this, ev); }; $1['events'].push(window['Anapo']['newEventCallback']($2, __wrapped, $3));"
  js_pushEvent :: JSVal -> Text -> GHCJS.Callback (JSVal -> JSVal -> IO ()) -> IO ()

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

foreign import javascript unsafe
  "$1['raw'] = $2"
  js_setRaw :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['element'] = $2"
  js_setElement :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['text'] = $2"
  js_setText :: JSVal -> Text -> IO ()

foreign import javascript unsafe
  "$1['callbacks'] = $2"
  js_setCallbacks :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1['mark'] = $2"
  js_setMark :: JSVal -> Text -> IO ()

#endif
