{-# LANGUAGE OverloadedStrings #-}
module Anapo.Render
  ( RenderedVDomNode
  , VDomPath
  , VDomPathSegment(..)
  , renderedVDomNodeDom
  , renderVirtualDom
  , reconciliateVirtualDom
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Monad (forM_, when, forM)
import qualified Data.DList as DList
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (catMaybes)
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
import Anapo.Logging

data RenderedVDomNode = RenderedVDomNode
  { rvdnVDom :: V.SomeVDomNode
  , rvdnDom :: DOM.Node
  , rvdnEvents :: [SomeSaferEventListener]
  , rvdnResetProperties :: HMS.HashMap V.ElementPropertyName ResetProperty
  -- ^ holds functions that can reset all the properties that
  -- have been set.
  , rvdnStyle :: V.ElementStyle
  , rvdnAttributes :: V.ElementAttributes
  }

renderedVDomNodeDom :: RenderedVDomNode -> DOM.Node
renderedVDomNodeDom = rvdnDom

newtype ResetProperty = ResetProperty
  { unResetProperty :: forall el. (DOM.IsElement el) => el -> DOM.JSM ()
  }

data SomeSaferEventListener = forall t e. (DOM.IsEventTarget t, DOM.IsEvent e) => SomeSaferEventListener
  { _sselName :: DOM.EventM.EventName t e
  , _sselEv :: DOM.EventM.SaferEventListener t e
  }

{-# INLINE emptyOverlay #-}
emptyOverlay :: V.SomeVDomNode -> DOM.Node -> V.Node RenderedVDomNode
emptyOverlay vdom dom = V.Node
  { nodeBody = RenderedVDomNode vdom dom mempty mempty mempty mempty
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

{-# INLINE addEvents #-}
addEvents ::
     (DOM.IsElement el)
  => el
  -> V.ElementEvents el
  -> DOM.JSM [SomeSaferEventListener]
addEvents el evts = forM (DList.toList evts) $ \(V.SomeEvent evtName evt) -> do
  safel <- DOM.EventM.newListener (do ev <- ask; lift (evt el ev))
  DOM.EventM.addListener el evtName safel False
  return (SomeSaferEventListener evtName safel)

{-# INLINE addProperties #-}
addProperties ::
     (DOM.IsElement el)
  => el
  -> (DOM.JSVal -> el)
  -> V.ElementProperties el
  -> HMS.HashMap V.ElementPropertyName ResetProperty -- ^ previous reset properties
  -> DOM.JSM (HMS.HashMap V.ElementPropertyName ResetProperty)
addProperties el wrap props prevReset =
  fmap (HMS.union prevReset . HMS.fromList . catMaybes) $
    forM (HMS.toList props) $ \(propName, V.ElementProperty{..}) -> do
      def <- eaGetProperty el
      eaSetProperty el =<< eaValue
      return $ case HMS.lookup propName prevReset of
        Nothing -> Just
          ( propName
          , ResetProperty $ \el' -> do
              el'' <- DOM.unsafeCastTo wrap el'
              eaSetProperty el'' def
          )
        Just{} -> Nothing

{-# INLINE addStyle #-}
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

{-# INLINE addAttributes #-}
addAttributes ::
     (DOM.IsElement el)
  => el
  -> V.ElementAttributes
  -> V.ElementAttributes
  -> DOM.JSM V.ElementAttributes
addAttributes el attrs prevAttrs = do
  -- remove all the attributes that are not there anymore
  for_ (HMS.keys (prevAttrs `HMS.difference` attrs)) (DOM.Element.removeAttribute el)
  -- add the others
  for_ (HMS.toList attrs) $ \(propName, prop) -> do
    let set = DOM.Element.setAttribute el propName prop
    case HMS.lookup propName prevAttrs of
      Nothing -> set
      Just prop' -> when (prop /= prop') set
  return attrs

{-# INLINE renderDom #-}
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

{-# INLINE renderChildren #-}
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

{-# INLINE renderNode #-}
renderNode ::
     DOM.Document
  -> V.Node V.SomeVDomNode -- ^ what to render
  -> (V.Node RenderedVDomNode -> DOM.JSM a)
  -- ^ the continuation should mount the rendered node into the dom.
  -- it's a continuation so that we can call the appropriate callbacks
  -- when the node is mounted.
  -> DOM.JSM a
renderNode doc V.Node{nodeBody = vdom@(V.SomeVDomNode V.VDomNode{..}), nodeChildren} cont0 = do
  let cont rendered = do
        V.callbacksUnsafeWillMount vdomCallbacks (rvdnDom (V.nodeBody rendered))
        x <- cont0 rendered
        V.callbacksUnsafeDidMount vdomCallbacks (rvdnDom (V.nodeBody rendered))
        return x
  case vdomBody of
    V.VDBText txt -> do
      txtNode <- DOM.Document.createTextNode doc txt
      cont (emptyOverlay vdom (DOM.toNode txtNode))
    V.VDBRawNode el -> cont (emptyOverlay vdom (DOM.toNode el))
    V.VDBElement V.Element{..} -> do
      el <- DOM.unsafeCastTo vdomWrap =<< DOM.Document.createElement doc elementTag
      defProps <- addProperties el vdomWrap elementProperties mempty
      attrs <- addAttributes el elementAttributes mempty
      style <- addStyle el elementStyle mempty
      evts <- addEvents el elementEvents
      childrenRendered <- case nodeChildren of
        Nothing -> fail "renderNode: got no children for an element"
        Just children -> renderChildren doc el children
      cont $ V.Node
        (RenderedVDomNode vdom (DOM.toNode el) evts defProps style attrs)
        (Just childrenRendered)

{-# INLINE renderVirtualDom #-}
renderVirtualDom ::
     V.Node V.SomeVDomNode
  -> (V.Node RenderedVDomNode -> DOM.JSM a)
  -- ^ this should mount the node
  -> DOM.JSM a
renderVirtualDom vdom mount = do
  doc <- DOM.currentDocumentUnchecked
  renderNode doc vdom mount

{-# INLINE reconciliateVirtualDom #-}
reconciliateVirtualDom ::
     V.Node RenderedVDomNode
  -> VDomPath
  -> V.Node V.SomeVDomNode
  -> DOM.JSM (V.Node RenderedVDomNode)
reconciliateVirtualDom prevVdom000 path000 vdom000 = do
  doc <- DOM.currentDocumentUnchecked
  let
    {-# INLINE removeChildren #-}
    removeChildren ::
         DOM.Node
      -> V.Children RenderedVDomNode
      -> DOM.JSM ()
    removeChildren container = \case
      V.CRawHtml{} -> return ()
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

    {-# INLINE removeNode #-}
    removeNode ::
         DOM.Node -- ^ the container
      -> V.Node RenderedVDomNode
      -> DOM.JSM ()
    removeNode
        container
        (V.Node RenderedVDomNode{rvdnVDom = V.SomeVDomNode V.VDomNode{..}, ..} mbChildren) = do
      -- first call the will remove
      V.callbacksUnsafeWillRemove vdomCallbacks rvdnDom
      -- then recurse down...
      for_ mbChildren (removeChildren rvdnDom)
      -- then remove the DOM thing and remove the children
      DOM.Node.removeChild_ container rvdnDom
      for_ rvdnEvents (\(SomeSaferEventListener _ ev) -> DOM.EventM.releaseListener ev)

    {-# INLINE patchDom #-}
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

    {-# INLINE patchKeyedDom #-}
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

    {-# INLINE patchMapDom #-}
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

    {-# INLINE patchChildren #-}
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

    {-# INLINE patchNode #-}
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
            V.callbacksUnsafeWillPatch (V.vdomCallbacks prevVDom) (rvdnDom prevRendered)
            dom' <- DOM.unsafeCastTo (V.vdomWrap vdom)  (rvdnDom prevRendered)
            -- reset properties that are gone to their default
            let removedProps = V.elementProperties prevElement `HMS.difference` V.elementProperties element
            -- TODO we could remove the default properties at this point, but i'm
            -- not sure it's even worth it
            forM_ (HMS.keys removedProps) $ \prop ->
              unResetProperty (rvdnResetProperties prevRendered HMS.! prop) dom'
            -- insert new properties, and augment the property resetters
            newResetProperties <- addProperties
              dom'
              (V.vdomWrap vdom)
              (V.elementProperties element)
              (rvdnResetProperties prevRendered)
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
            -- add all events
            evts <- addEvents dom' (V.elementEvents element)
            -- patch children
            newChildren <- case (prevChildren, children) of
              (Just prevChildren', Just children') -> patchChildren dom' prevChildren' children'
              (_, _) -> fail "renderVirtualDom.patchNode: no children for Element!"
            -- callback after patch
            V.callbacksUnsafeDidPatch (V.vdomCallbacks vdom) (rvdnDom prevRendered)
            return V.Node
              { V.nodeBody = RenderedVDomNode
                  { rvdnVDom = V.SomeVDomNode vdom
                  , rvdnDom = rvdnDom prevRendered
                  , rvdnEvents = evts
                  , rvdnResetProperties = newResetProperties
                  , rvdnStyle = newStyle
                  , rvdnAttributes = newAttrs
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
