{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
module Anapo.Render
  ( RenderedVDomNode
  , VDomPath
  , VDomPathSegment(..)
  , renderedVDomNodeDom
  , renderVirtualDom
  , reconciliateVirtualDom
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Monad (forM_, when, forM, unless)
import qualified Data.DList as DList
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Foldable (for_)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))
import Data.Traversable (for)
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Hashable (Hashable(..))
import GHC.Stack (HasCallStack)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Control.Monad (join)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM.Document
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM.Node
import qualified GHCJS.DOM.Element as DOM.Element
import qualified GHCJS.DOM.CSSStyleDeclaration as DOM.CSSStyleDeclaration
import qualified GHCJS.DOM.ElementCSSInlineStyle as DOM.ElementCSSInlineStyle
import qualified GHCJS.DOM.EventM as DOM.EventM

import qualified Anapo.VDOM as V
import qualified Anapo.OrderedHashMap as OHM
import Anapo.Text (Text, pack)
import qualified Anapo.Text as T
import Anapo.Logging

#if defined(ghcjs_HOST_OS)
import Data.JSString (JSString)
import qualified GHC.Exts as Exts
import Control.DeepSeq (rnf)
#else
import qualified Language.Javascript.JSaddle as JSaddle
import Control.Lens ((^.))
import Control.Monad (void)
#endif

-- | we need to keep default properties to be able to remove them
data ExistingProperty = ExistingProperty
  { epDefault :: V.ElementProperty
  , epCurrent :: V.ElementProperty
  }

data RenderedVDomNode = RenderedVDomNode
  { rvdnVDom :: V.SomeVDomNode
  , rvdnDom :: DOM.Node
  , rvdnEvents :: [SomeSaferEventListener]
  , rvdnProperties :: HMS.HashMap V.ElementPropertyName ExistingProperty
  , rvdnStyle :: V.ElementStyle
  , rvdnAttributes :: V.ElementAttributes
  , rvdnClasses :: V.ElementClasses
  }

{-# INLINABLE renderedVDomNodeDom #-}
renderedVDomNodeDom :: RenderedVDomNode -> DOM.Node
renderedVDomNodeDom = rvdnDom

data SomeSaferEventListener =
  forall t e. (DOM.IsEventTarget t, DOM.IsEvent e) => SomeSaferEventListener
    { _sselName :: DOM.EventM.EventName t e
    , _sselEv :: DOM.EventM.SaferEventListener t e
    }

{-# INLINE emptyOverlay #-}
emptyOverlay :: V.SomeVDomNode -> DOM.Node -> V.Node RenderedVDomNode
emptyOverlay vdom dom = V.Node
  { nodeBody = RenderedVDomNode vdom dom mempty mempty mempty mempty mempty
  , nodeChildren = Nothing
  }

type VDomPath = [VDomPathSegment]

data VDomPathSegment =
    VDPSNormal Int
  | VDPSKeyed Text
  deriving (Eq, Show)
instance Hashable VDomPathSegment where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (VDPSNormal i) = salt `hashWithSalt` (0::Int) `hashWithSalt` i
  hashWithSalt salt (VDPSKeyed i) = salt `hashWithSalt` (1::Int) `hashWithSalt` i

addEvents ::
     (DOM.IsElement el)
  => el
  -> V.ElementEvents el
  -> DOM.JSM [SomeSaferEventListener]
addEvents el evts = forM (DList.toList evts) $ \(V.SomeEvent evtName evt) -> do
  -- have listeners to be sync -- it's faster and we really do not
  -- people to run blocking calls in them
  safel <- DOM.EventM.newListenerSync (do ev <- ask; lift (evt el ev))
  DOM.EventM.addListener el evtName safel False
  return (SomeSaferEventListener evtName safel)

addProperties ::
     (DOM.ToJSVal el)
  => el
  -> V.ElementProperties
  -> HMS.HashMap V.ElementPropertyName ExistingProperty
  -> DOM.JSM (HMS.HashMap V.ElementPropertyName ExistingProperty)
addProperties el0 props prevProps = do
  el <- DOM.toJSVal el0
  -- remove all the properties that are not there anymore
  for_
    (HMS.toList (prevProps `HMS.difference` props))
    (\(k, ep) -> setProperty el k (epDefault ep))
  -- add the others
  fmap HMS.fromList $ for (HMS.toList props) $ \(propName, prop) -> do
    let set = setProperty el propName prop
    case HMS.lookup propName prevProps of
      Nothing -> do
        val <- getProperty el propName
        set
        return
          ( propName
          , ExistingProperty
              { epDefault = val
              , epCurrent = prop
              }
          )
      Just ep -> do
        eq <- jsEq prop (epCurrent ep)
        unless eq set
        return (propName, ep{epCurrent = prop})

addStyle ::
     (DOM.IsElementCSSInlineStyle el)
  => el
  -> V.ElementStyle
  -> V.ElementStyle -- ^ previous style properties
  -> DOM.JSM V.ElementStyle
addStyle el style prevStyle = do
  css <- DOM.ElementCSSInlineStyle.getStyle el
  -- remove all the styles that are not there anymore
  for_ (HMS.keys (prevStyle `HMS.difference` style)) (DOM.CSSStyleDeclaration.removeProperty_ css)
  -- add the others
  for_ (HMS.toList style) $ \(propName, prop) -> do
    let set = DOM.CSSStyleDeclaration.setProperty css propName prop (Nothing :: Maybe Text)
    case HMS.lookup propName prevStyle of
      Nothing -> set
      Just prop' -> when (prop /= prop') set
  return style

{-# INLINE addClasses #-}
addClasses ::
     (DOM.IsElement el)
  => el
  -> V.ElementClasses
  -> V.ElementClasses
  -> DOM.JSM V.ElementClasses
addClasses el classes0 prevClasses = do
  let classes = filter (not . T.null) classes0 -- add barfs on empty strings
  toks <- DOM.Element.getClassList el
  tokenListRemove toks prevClasses
  tokenListAdd toks classes
  return classes

addAttributes ::
     (DOM.IsElement el)
  => el
  -> V.ElementAttributes
  -> V.ElementAttributes
  -> DOM.JSM V.ElementAttributes
addAttributes el attrs prevAttrs = do
  -- remove all the attributes that are not there anymore
  for_
    (HMS.keys (prevAttrs `HMS.difference` attrs))
    (DOM.Element.removeAttribute el)
  -- add the others
  for_ (HMS.toList attrs) $ \(propName, prop) -> do
    let set = do
          val <- DOM.toJSVal el
          setAttribute val propName prop
    case HMS.lookup propName prevAttrs of
      Nothing -> set
      Just prop' -> do
        eq <- jsEq prop prop'
        unless eq set
  return attrs

renderDom ::
     (DOM.IsNode el, Traversable t)
  => DOM.Document
  -> el -- ^ container
  -> t (V.Node V.SomeVDomNode)
  -> DOM.JSM (t (V.Node RenderedVDomNode))
renderDom doc container vdoms =
  -- KEEP IN SYNC WITH patchKeyedDom
  for vdoms $ \vdom -> do
    renderNode doc vdom $ \rendered -> do
      DOM.Node.appendChild_ container (rvdnDom (V.nodeBody rendered))
      return rendered

renderChildren ::
     (DOM.IsElement el)
  => DOM.Document
  -> el -- ^ container
  -> V.Children V.SomeVDomNode
  -> DOM.JSM (V.Children RenderedVDomNode)
renderChildren doc container = \case
  V.CRawHtml html -> do
    DOM.Element.setInnerHTML container html
    return (V.CRawHtml html)
  V.CNormal vdoms ->
    V.CNormal <$> renderDom doc container vdoms
  V.CKeyed vdoms ->
    V.CKeyed <$> renderDom doc container vdoms
  V.CMap vdoms ->
    V.CMap <$> renderDom doc container vdoms

renderNode ::
     DOM.Document
  -> V.Node V.SomeVDomNode -- ^ what to render
  -> (V.Node RenderedVDomNode -> DOM.JSM a)
  -- ^ the continuation should mount the rendered node into the dom.
  -- it's a continuation so that we can call the appropriate callbacks
  -- when the node is mounted.
  -> DOM.JSM a
renderNode doc V.Node{nodeBody = vdom@(V.SomeVDomNode V.VDomNode{..}), nodeChildren} cont0 = do
  let cont el rendered = do
        V.callbacksWillMount vdomCallbacks el
        x <- cont0 rendered
        V.callbacksDidMount vdomCallbacks el
        return x
  case vdomBody of
    V.VDBText txt -> do
      txtNode <- DOM.Document.createTextNode doc txt
      cont txtNode (emptyOverlay vdom (DOM.toNode txtNode))
    V.VDBRawNode el -> cont el (emptyOverlay vdom (DOM.toNode el))
    V.VDBElement V.Element{..} -> do
      el <- DOM.unsafeCastTo vdomWrap =<< DOM.Document.createElement doc elementTag
      defProps <- addProperties el elementProperties mempty
      attrs <- addAttributes el elementAttributes mempty
      style <- addStyle el elementStyle mempty
      classes <- addClasses el elementClasses mempty
      evts <- addEvents el elementEvents
      childrenRendered <- case nodeChildren of
        Nothing -> fail "renderNode: got no children for an element"
        Just children -> renderChildren doc el children
      cont el $ V.Node
        (RenderedVDomNode vdom (DOM.toNode el) evts defProps style attrs classes)
        (Just childrenRendered)

renderVirtualDom ::
     V.Node V.SomeVDomNode
  -> (V.Node RenderedVDomNode -> DOM.JSM a)
  -- ^ this should mount the node
  -> DOM.JSM a
renderVirtualDom vdom mount = do
  doc <- DOM.currentDocumentUnchecked
  renderNode doc vdom mount

reconciliateVirtualDom ::
     V.Node RenderedVDomNode
  -> VDomPath
  -> V.Node V.SomeVDomNode
  -> DOM.JSM (V.Node RenderedVDomNode)
reconciliateVirtualDom prevVdom000 path000 vdom000 = do
  doc <- DOM.currentDocumentUnchecked
  let
    removeChildren ::
         DOM.Node
      -> V.Children RenderedVDomNode
      -> DOM.JSM ()
    removeChildren container = \case
      V.CRawHtml{} -> do
        container' <- DOM.unsafeCastTo DOM.Element container
        DOM.Element.setInnerHTML container' ("" :: Text)
      V.CNormal vdoms -> removeDom container vdoms
      V.CKeyed vdoms -> removeDom container vdoms
      V.CMap vdoms -> removeDom container vdoms

    {-# INLINE removeDom #-}
    removeDom ::
         (Foldable t)
      => DOM.Node
      -> t (V.Node RenderedVDomNode)
      -> DOM.JSM ()
    removeDom container vdoms = for_ vdoms (removeNode container)

    -- | collect event listeners to free and call willRemove, but do not
    -- remove the DOM.
    cleanupNode ::
         IORef (DOM.JSM ()) -- ^ how to cleanup the listeners
      -> V.Node RenderedVDomNode
      -> DOM.JSM ()
    cleanupNode
        releaseListenersRef
        (V.Node RenderedVDomNode{rvdnVDom = V.SomeVDomNode V.VDomNode{..}, ..} mbChildren) = do
      -- first call the will remove
      V.callbacksWillRemove vdomCallbacks =<< DOM.unsafeCastTo vdomWrap rvdnDom
      -- then recurse down
      for_ mbChildren (cleanupChildren releaseListenersRef)
      -- record how to cleanup the listeners
      unless (null rvdnEvents) $ liftIO $ modifyIORef' releaseListenersRef $ \m -> do
        m
        for_ rvdnEvents (\(SomeSaferEventListener _ ev) -> DOM.EventM.releaseListener ev)

    cleanupChildren ::
         IORef (DOM.JSM ()) -- ^ how to cleanup the listeners
      -> V.Children RenderedVDomNode
      -> DOM.JSM ()
    cleanupChildren releaseListenersRef = \case
      V.CRawHtml{} -> return ()
      V.CNormal vdoms -> cleanupDom releaseListenersRef vdoms
      V.CKeyed vdoms -> cleanupDom releaseListenersRef vdoms
      V.CMap vdoms -> cleanupDom releaseListenersRef vdoms

    {-# INLINE cleanupDom #-}
    cleanupDom ::
         (Foldable t)
      => IORef (DOM.JSM ())
      -> t (V.Node RenderedVDomNode)
      -> DOM.JSM ()
    cleanupDom releaseListenersRef vdoms = for_ vdoms (cleanupNode releaseListenersRef)

    removeNode ::
         DOM.Node -- ^ the container
      -> V.Node RenderedVDomNode
      -> DOM.JSM ()
    removeNode
        container
        (V.Node RenderedVDomNode{rvdnVDom = V.SomeVDomNode V.VDomNode{..}, ..} mbChildren) = do
      -- first call the will remove
      V.callbacksWillRemove vdomCallbacks =<< DOM.unsafeCastTo vdomWrap rvdnDom
      -- cleanup the children
      releaseListenersRef <- liftIO (newIORef (return ()))
      for_ mbChildren (cleanupChildren releaseListenersRef)
      -- then remove the DOM element and clean up the listeners
      DOM.Node.removeChild_ container rvdnDom
      join (liftIO (readIORef releaseListenersRef))
      for_ rvdnEvents (\(SomeSaferEventListener _ ev) -> DOM.EventM.releaseListener ev)

    patchDom ::
         DOM.Node -- ^ the container
      -> Vector (V.Node RenderedVDomNode) -- ^ the previous vdom
      -> Vector (V.Node V.SomeVDomNode) -- ^ the current vdom
      -> DOM.JSM (Vector (V.Node RenderedVDomNode))
    patchDom container prevVDoms0 vdoms0 = do
      let
        go prevVDoms vdoms = case (prevVDoms, vdoms) of
          ([], vdoms') -> renderDom doc container vdoms'
          (prevVDoms', []) -> do
            removeDom container prevVDoms'
            return []
          (prevVDom : prevVDoms', vdom : vdoms') -> do
            rendered <- patchNode prevVDom vdom
            rendereds <- go prevVDoms' vdoms'
            return (rendered : rendereds)
      Vec.fromList <$> go (Vec.toList prevVDoms0) (Vec.toList vdoms0)

    patchKeyedDom ::
         DOM.Node -- ^ the container
      -> OHM.OrderedHashMap Text (V.Node RenderedVDomNode) -- ^ the previous vdom
      -> OHM.OrderedHashMap Text (V.Node V.SomeVDomNode) -- ^ the current vdom
      -> DOM.JSM (OHM.OrderedHashMap Text (V.Node RenderedVDomNode))
    patchKeyedDom container prevVDoms0 vdoms0 = do
      let
        go ::
             [Text] -- the previous keys left to visit
          -> HMS.HashMap Text (V.Node RenderedVDomNode) -- the previous nodes _still in their original positions_
          -> [Text] -- the new keys left to visit
          -> Maybe DOM.Node
          -> DOM.JSM [(Text, V.Node RenderedVDomNode)]
        go prevKs0 prevNodesUntouched ks mbCursor = do
          let prevKs = dropWhile (\k -> not (HMS.member k prevNodesUntouched)) prevKs0
          case ks of
            [] -> do
              -- we want to remove them in the right order.
              let prevKsUntouched = filter (`HMS.member` prevNodesUntouched) prevKs
              removeDom container (OHM.unsafeNew prevNodesUntouched (Vec.fromList prevKsUntouched))
              return []
            k : ks' -> case prevKs of
              [] -> do
                for (k : ks') $ \ix ->
                  renderNode doc (vdoms0 OHM.! ix) $ \rendered -> do
                    DOM.Node.appendChild_ container (rvdnDom (V.nodeBody rendered))
                    return (ix, rendered)
              -- this one below is a special but common case (the dom
              -- didn't change at all). it should be removable without
              -- loss of correctness.
              prevK : prevKs' | prevK == k -> do
                rendered <- patchNode (prevNodesUntouched HMS.! k) (vdoms0 OHM.! k)
                rendereds <- go prevKs' (HMS.delete k prevNodesUntouched) ks' (Just (rvdnDom (V.nodeBody rendered)))
                return ((k, rendered) : rendereds)
              _:_ ->
                case HMS.lookup k prevNodesUntouched of
                  Nothing -> do
                    rendered <- renderNode doc (vdoms0 OHM.! k) $ \rendered -> do
                      appendAfter_ container mbCursor (rvdnDom (V.nodeBody rendered))
                      return rendered
                    rendereds <- go prevKs prevNodesUntouched ks' (Just (rvdnDom (V.nodeBody rendered)))
                    return ((k, rendered) : rendereds)
                  Just prevNode -> do
                    -- move the node right after the cursor
                    -- (appendAfter_ will remove it from the previous
                    -- position automatically)
                    appendAfter_ container mbCursor (rvdnDom (V.nodeBody prevNode))
                    rendered <- patchNode prevNode (vdoms0 OHM.! k)
                    rendereds <- go prevKs (HMS.delete k prevNodesUntouched) ks' (Just (rvdnDom (V.nodeBody rendered)))
                    return ((k, rendered) : rendereds)
      hm <-
        fmap HMS.fromList $
          go (Vec.toList (OHM.getOrder prevVDoms0)) (OHM.getMap prevVDoms0) (Vec.toList (OHM.getOrder vdoms0)) Nothing
      return (OHM.unsafeNew hm (OHM.getOrder vdoms0))

    patchMapDom ::
         DOM.Node -- ^ the container
      -> HMS.HashMap Text (V.Node RenderedVDomNode) -- ^ the previous vdom
      -> HMS.HashMap Text (V.Node V.SomeVDomNode) -- ^ the current vdom
      -> DOM.JSM (HMS.HashMap Text (V.Node RenderedVDomNode))
    patchMapDom container prevVDoms0 vdoms0 = do
      let
        go ::
             HMS.HashMap Text (V.Node RenderedVDomNode) -- ^ the previous nodes left to patch
          -> [(Text, V.Node V.SomeVDomNode)] -- ^ the remaining new nodes to analyze
          -> DOM.JSM [(Text, V.Node RenderedVDomNode)]
        go prevVDoms = \case
          [] -> do
            removeDom container prevVDoms
            return []
          (k, vdom) : vdoms -> case HMS.lookup k prevVDoms of
            Nothing -> do
              rendered <- renderNode doc vdom $ \rendered -> do
                DOM.Node.appendChild_ container (rvdnDom (V.nodeBody rendered))
                return rendered
              rendereds <- go prevVDoms vdoms
              return ((k, rendered) : rendereds)
            Just rendered -> do
              rendered' <- patchNode rendered vdom
              rendereds <- go (HMS.delete k prevVDoms) vdoms
              return ((k, rendered') : rendereds)
      fmap HMS.fromList (go prevVDoms0 (HMS.toList vdoms0))

    patchChildren ::
         (DOM.IsElement el)
      => el -- ^ the containing node
      -> V.Children RenderedVDomNode -- ^ previous children
      -> V.Children V.SomeVDomNode -- ^ current children
      -> DOM.JSM (V.Children RenderedVDomNode)
    patchChildren container prevChildren0 children0 =
      case (prevChildren0, children0) of
        -- TODO consider ref equality for HTML
        (V.CRawHtml prevHtml, V.CRawHtml html) | prevHtml == html -> return (V.CRawHtml prevHtml)
        (V.CNormal prevNChildren, V.CNormal nchildren) ->
          V.CNormal <$> patchDom (DOM.toNode container) prevNChildren nchildren
        (V.CKeyed prevNChildren, V.CKeyed nchildren) ->
          V.CKeyed <$> patchKeyedDom (DOM.toNode container) prevNChildren nchildren
        (V.CMap prevNChildren, V.CMap nchildren) ->
          V.CMap <$> patchMapDom (DOM.toNode container) prevNChildren nchildren
        (prevChildren, children) -> do
          removeChildren (DOM.toNode container) prevChildren
          renderChildren doc container children

    patchNode ::
         V.Node RenderedVDomNode
      -> V.Node V.SomeVDomNode
      -> DOM.JSM (V.Node RenderedVDomNode)
    patchNode
        prevNode@(V.Node prevRendered@RenderedVDomNode{rvdnVDom = V.SomeVDomNode prevVDom} prevChildren)
        node@(V.Node (V.SomeVDomNode vdom) children) = do
      -- check if they're both marked and without the rerender also note
      -- that if we switch from marked to unmarked, or if the fingerprints
      -- differ, we always assume they're incompatible, to speed up
      -- deletion (especially on jssaddle)
      case (V.vdomMark prevVDom, V.vdomMark vdom) of
        (Just (V.Mark prevFprint _), Just (V.Mark fprint rerender)) ->
          if prevFprint == fprint
            then case rerender of
              V.UnsafeDontRerender -> return prevNode
              V.Rerender -> patch
            else incompatible
        (Just{}, Nothing) -> incompatible
        (Nothing, Just{}) -> incompatible
        (Nothing, Nothing) -> patch
      where
        patch = case (V.vdomBody prevVDom, V.vdomBody vdom) of
          -- Text
          -- TODO consider ref. equality, also for rawnode
          -- TODO consider asserting no events when appropriate
          (V.VDBText prevTxt, V.VDBText txt) | prevTxt == txt ->
            return prevNode
          -- Element
          (V.VDBElement prevElement, V.VDBElement element) | V.elementTag prevElement == V.elementTag element -> do
            -- callback before patch
            V.callbacksWillPatch (V.vdomCallbacks prevVDom) =<<
              DOM.unsafeCastTo (V.vdomWrap prevVDom) (rvdnDom prevRendered)
            dom' <- DOM.unsafeCastTo (V.vdomWrap vdom) (rvdnDom prevRendered)
            -- add props
            newProps <- addProperties
              dom'
              (V.elementProperties element)
              (rvdnProperties prevRendered)
            -- remove all events
            -- TODO we should probably have an option to be able to
            -- have stable events, so that we do not have to delete
            -- everything each time
            -- TODO can it be that changing the attributes triggers some
            -- events? we should check
            forM_ (rvdnEvents prevRendered) $ \(SomeSaferEventListener evtName evt) -> do
              -- TODO maybe do something safer here...
              DOM.EventM.removeListener (unsafeCoerce dom') evtName evt False
              DOM.EventM.releaseListener evt
            -- add attributes
            newAttrs <- addAttributes dom' (V.elementAttributes element) (rvdnAttributes prevRendered)
            -- add style
            newStyle <- addStyle dom' (V.elementStyle element) (rvdnStyle prevRendered)
            newClasses <- addClasses dom' (V.elementClasses element) (rvdnClasses prevRendered)
            -- add all events
            evts <- addEvents dom' (V.elementEvents element)
            -- patch children
            newChildren <- case (prevChildren, children) of
              (Just prevChildren', Just children') -> patchChildren dom' prevChildren' children'
              (_, _) -> fail "renderVirtualDom.patchNode: no children for Element!"
            -- callback after patch
            V.callbacksDidPatch (V.vdomCallbacks vdom) dom'
            return V.Node
              { V.nodeBody = RenderedVDomNode
                  { rvdnVDom = V.SomeVDomNode vdom
                  , rvdnDom = rvdnDom prevRendered
                  , rvdnEvents = evts
                  , rvdnProperties = newProps
                  , rvdnStyle = newStyle
                  , rvdnAttributes = newAttrs
                  , rvdnClasses = newClasses
                  }
              , V.nodeChildren = Just newChildren
              }
          -- In all other cases we erase and rerender
          _ ->
            incompatible

        incompatible = do
          -- remove all chidren and swap the topmost node
          container <- DOM.Node.getParentNodeUnsafe (rvdnDom prevRendered)
          -- it is important that we remove before we mount, otherwise
          -- the willRemove will be called before the didMount, which
          -- is bizarre and less useful -- for example it prevents us
          -- from implementing components. to do that, we first save the
          -- next node to know where to insert.
          mbPrevChild <- DOM.Node.getNextSibling (rvdnDom (V.nodeBody prevNode))
          removeNode container prevNode
          renderNode doc node $ \rendered -> do
            DOM.Node.insertBefore_ container (rvdnDom (V.nodeBody rendered)) mbPrevChild
            return rendered

    followPath ::
         V.Node RenderedVDomNode
      -> (V.Node RenderedVDomNode -> DOM.JSM (V.Node RenderedVDomNode))
      -> VDomPath
      -> DOM.JSM (V.Node RenderedVDomNode)
    followPath node f = \case
      [] -> f node
      ix : path -> case V.nodeChildren node of
        Nothing -> fail ("renderVirtualDom: no children when following non-empty path: " <> show path)
        Just ch -> case ch of
          V.CRawHtml{} -> fail "renderVirtualDom: CRawHtml when following non-empty path"
          V.CNormal children -> case ix of
            VDPSKeyed{} -> fail "renderVirtualDom: got keyed path segment for normal dom"
            VDPSNormal ix' -> case children Vec.!? ix' of
              Nothing -> fail ("renderVirtualDom: did not find index " <> show ix <> " when following path")
              Just n -> do
                n' <- followPath n f path
                return node{ V.nodeChildren = Just (V.CNormal (children Vec.// [(ix', n')])) }
          V.CKeyed children -> case ix of
            VDPSNormal{} -> fail "renderVirtualDom: got normal path segment for keyed dom"
            VDPSKeyed ix' -> do
              n' <- followPath (children OHM.! ix') f path
              return node{ V.nodeChildren = Just (V.CKeyed (OHM.replace ix' n' children)) }
          V.CMap children -> case ix of
            VDPSNormal{} -> fail "renderVirtualDom: got normal path segment for map dom"
            VDPSKeyed ix' -> do
              n' <- followPath (children HMS.! ix') f path
              return node{ V.nodeChildren = Just (V.CMap (HMS.insert ix' n' children)) }

  t0 <- liftIO getCurrentTime
  x <- followPath prevVdom000 (\pathVdom -> patchNode pathVdom vdom000) path000
  t1 <- liftIO getCurrentTime
  logDebug ("Vdom reconciled (" <> pack (show (diffUTCTime t1 t0)) <> ")")
  return x

-- if the cursor is null, the node will be inserted at the beginning.
appendAfter_ :: HasCallStack => DOM.Node -> Maybe DOM.Node -> DOM.Node -> DOM.JSM ()
appendAfter_ container mbCursor node = case mbCursor of
  Nothing -> do
    -- if the cursor is null, put before the first element. note that if
    -- there is no first element it'll just append the node, which is
    -- fine since it's the only node.
    DOM.Node.insertBefore_ container node =<< DOM.Node.getFirstChild container
  Just cursor -> do
    -- if the cursor has no next sibling it'll append it at the end,
    -- which is what we want.
    DOM.Node.insertBefore_ container node =<< DOM.Node.getNextSibling cursor

-- JS equality for properties / attributes / ...
-----------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)
foreign import javascript unsafe
  "h$anapoJsEq($1, $2)"
  jsEq :: DOM.JSVal -> DOM.JSVal -> DOM.JSM Bool
#else
jsEq :: DOM.JSVal -> DOM.JSVal -> DOM.JSM Bool
jsEq _ _ = return False -- this is an optimization anyway.
#endif

-- setAttribute with JSVal rather than Text. this is for stuff like
-- aframe that has elements which have attributes that are _not_ text,
-- e.g. rotations and such.
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute :: DOM.JSVal -> Text -> DOM.JSVal -> DOM.JSM ()
#else
setAttribute :: DOM.JSVal -> Text -> DOM.JSVal -> DOM.JSM ()
setAttribute val k v = do
  kval <- DOM.toJSVal k
  void (val ^. JSaddle.jsf ("setAttribute" :: Text) [kval, v])
#endif

-- setting properties
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "$1[$2]"
  getProperty :: DOM.JSVal -> Text -> DOM.JSM DOM.JSVal

foreign import javascript unsafe
  "$1[$2] = $3"
  setProperty :: DOM.JSVal -> Text -> DOM.JSVal -> DOM.JSM ()

#else

getProperty :: DOM.JSVal -> Text -> DOM.JSM DOM.JSVal
getProperty el k = el JSaddle.! k

setProperty :: DOM.JSVal -> Text -> DOM.JSVal -> DOM.JSM ()
setProperty el k x = (el JSaddle.<# k) x

#endif

-- token list (the ghcjs one seems to be broken, passes the list in
-- rather than as arguments)
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "$1.add.apply($1, h$fromHsListJSVal($2))"
 js_tokenListAdd :: DOM.JSVal -> Exts.Any -> IO ()

tokenListAdd :: DOM.DOMTokenList -> [JSString] -> IO ()
tokenListAdd (DOM.DOMTokenList x) ss = rnf ss `seq` js_tokenListAdd x (unsafeCoerce ss)

foreign import javascript unsafe
  "$1.remove.apply($1, h$fromHsListJSVal($2))"
 js_tokenListRemove :: DOM.JSVal -> Exts.Any -> IO ()

tokenListRemove :: DOM.DOMTokenList -> [JSString] -> IO ()
tokenListRemove (DOM.DOMTokenList x) ss = rnf ss `seq` js_tokenListRemove x (unsafeCoerce ss)

#else

tokenListAdd :: DOM.DOMTokenList -> [Text] -> DOM.JSM ()
tokenListAdd x vals = void (x ^. JSaddle.jsf ("add" :: Text) vals)

tokenListRemove :: DOM.DOMTokenList -> [Text] -> DOM.JSM ()
tokenListRemove x vals = void (x ^. JSaddle.jsf ("remove" :: Text) vals)

#endif

