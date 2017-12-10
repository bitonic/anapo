{-# LANGUAGE OverloadedStrings #-}
module Anapo.Render (Overlay, renderVirtualDom) where

import qualified Data.HashMap.Strict as HMS
import Control.Monad (forM_, when, forM)
import qualified Data.DList as DList
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (catMaybes)
import Data.Foldable (for_)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))

import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.CSSStyleDeclaration as DOM.CSSStyleDeclaration
import qualified GHCJS.DOM.ElementCSSInlineStyle as DOM.ElementCSSInlineStyle
import qualified GHCJS.DOM.EventM as DOM

import qualified Anapo.VDOM as V
import Anapo.Text (Text, pack)
import Anapo.Logging

type Overlay = [NodeOverlay]

newtype ResetProperty = ResetProperty
  { resetProperty :: forall el. (DOM.IsElement el) => el -> DOM.JSM ()
  }

data SomeSaferEventListener = forall t e. (DOM.IsEventTarget t, DOM.IsEvent e) => SomeSaferEventListener
  { _sselName :: DOM.EventName t e
  , _sselEv :: DOM.SaferEventListener t e
  }

data NodeOverlay = NodeOverlay
  { noEvents :: [SomeSaferEventListener]
  , noResetProperties :: HMS.HashMap V.ElementPropertyName ResetProperty
  -- ^ holds functions that can reset all the properties that
  -- have been set.
  , noStyle :: V.ElementStyle
  , noChildren :: Overlay
  , noNode :: DOM.Node
  }

{-# INLINE emptyOverlay #-}
emptyOverlay :: DOM.Node -> NodeOverlay
emptyOverlay = NodeOverlay mempty mempty mempty mempty

{-# INLINE renderVirtualDom #-}
renderVirtualDom :: forall el0.
     (DOM.IsElement el0)
  => DOM.Document
  -> el0
  -> Maybe (V.Dom, Overlay)
  -> V.Dom
  -> DOM.JSM Overlay
renderVirtualDom doc = let
  {-# INLINE eraseAllChildren #-}
  eraseAllChildren :: (DOM.IsNode el) => el -> DOM.JSM ()
  eraseAllChildren el = let
    go = do
      mbChild <- DOM.getFirstChild el
      forM_ mbChild $ \child -> do
        DOM.removeChild_ el child
        go
    in go

  {-# INLINE removeDomNodeChildren #-}
  removeDomNodeChildren ::
       DOM.Node
    -> Overlay
    -> V.Children
    -> DOM.JSM ()
  removeDomNodeChildren container overlay children = case children of
    V.CRawHtml{} -> if null overlay
      then return ()
      else fail "eraseDomNodeChildren: expecting no overlay nodes with rawhtml, but got some"
    V.CKeyed kvd -> removeDom container overlay (DList.toList (V.unkeyDom kvd))
    V.CNormal vdom -> removeDom container overlay (DList.toList vdom)

  {-# INLINE removeDom #-}
  removeDom :: DOM.Node -> Overlay -> [V.SomeNode] -> DOM.JSM ()
  removeDom container00 overlayNodes00 nodes00 = do
    let
      go container overlayNodes0 nodes0 = case (overlayNodes0, nodes0) of
        ([], []) -> return ()
        (nodeOverlay : overlayNodes, node : nodes) -> do
          removeNode container DOM.removeChild_ nodeOverlay node
          go container overlayNodes nodes
        ([], _:_) -> do
          fail "removeDom: got nodes but no overlay!"
        (_:_, []) -> do
          fail "removeDom: got overlay but no nodes!"
    go container00 overlayNodes00 nodes00

  {-# INLINE removeNode #-}
  removeNode ::
       DOM.Node -- ^ container node
    -> (DOM.Node -> DOM.Node -> DOM.JSM ())
    -- ^ how to remove the child node given a container. note that
    -- sometimes we replace the child with another child, which is why
    -- this is not always 'DOM.removeChild_'
    -> NodeOverlay
    -> V.SomeNode
    -> DOM.JSM ()
  removeNode container removeChild NodeOverlay{..} (V.SomeNode V.Node{..}) = do
    -- first call the will remove
    V.callbacksUnsafeWillRemove nodeCallbacks =<< DOM.unsafeCastTo nodeWrap noNode
    -- then recurse down...
    case nodeBody of
      V.NBElement el -> removeDomNodeChildren noNode noChildren (V.elementChildren el)
      -- TODO consider asserting that noChildren is null
      V.NBText{} -> return ()
      V.NBRawNode{} -> return ()
    -- then remove the DOM thing and remove the children
    removeChild container noNode
    for_ noEvents (\(SomeSaferEventListener _ ev) -> DOM.releaseListener ev)

  {-# INLINE addEvents #-}
  addEvents ::
       (DOM.IsElement el)
    => el
    -> V.ElementEvents el
    -> DOM.JSM [SomeSaferEventListener]
  addEvents el evts = forM (DList.toList evts) $ \(V.SomeEvent evtName evt) -> do
    safel <- DOM.newListener (do ev <- ask; lift (evt el ev))
    DOM.addListener el evtName safel False
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

  {-# INLINE renderDomChildren #-}
  renderDomChildren ::
       (DOM.IsElement el)
    => el -- ^ container
    -> V.Children
    -> DOM.JSM Overlay
  renderDomChildren container = \case
    V.CRawHtml html -> do
      DOM.setInnerHTML container html
      return mempty
    V.CKeyed kvd -> do
      -- TODO when we do implement keyed properly, check that the list
      -- size corresponds to the hasmap size
      renderDom container (DList.toList (V.unkeyDom kvd))
    V.CNormal vdom -> renderDom container (DList.toList vdom)

  {-# INLINE renderDomNode #-}
  renderDomNode ::
       V.SomeNode
    -> (forall el. (DOM.IsNode el) => el -> NodeOverlay -> DOM.JSM a)
    -> DOM.JSM a
  renderDomNode (V.SomeNode V.Node{..}) cont0 = do
    let cont el callbacks evts = do
          V.callbacksUnsafeWillMount callbacks el
          x <- cont0 el evts
          V.callbacksUnsafeDidMount callbacks el
          return x
    case nodeBody of
      V.NBText txt -> do
        txtNode <- DOM.createTextNode doc txt
        cont txtNode nodeCallbacks (emptyOverlay (DOM.toNode txtNode))
      V.NBRawNode el -> cont el nodeCallbacks (emptyOverlay (DOM.toNode el))
      V.NBElement V.Element{..} -> do
        el <- DOM.unsafeCastTo nodeWrap =<< DOM.createElement doc elementTag
        defProps <- addProperties el nodeWrap elementProperties mempty
        style <- addStyle el elementStyle mempty
        evts <- addEvents el elementEvents
        childrenEvents <- renderDomChildren el elementChildren
        cont el nodeCallbacks (NodeOverlay evts defProps style childrenEvents (DOM.toNode el))

  {-# INLINE renderDom #-}
  renderDom ::
       (DOM.IsNode el)
    => el -- ^ the node that should contain the dom
    -> [V.SomeNode]
    -> DOM.JSM [NodeOverlay]
  renderDom container = mapM $ \node@(V.SomeNode V.Node{..}) -> do
    renderDomNode node $ \el evts -> do
      DOM.appendChild_ container el
      return evts

  {-# INLINE patchDomChildren #-}
  patchDomChildren ::
       (DOM.IsElement el)
    => el -- ^ the containing node
    -> V.Children -- ^ previous children
    -> Overlay -- ^ the overlay for previous children
    -> V.Children -- ^ current children
    -> DOM.JSM Overlay
  patchDomChildren container prevChildren0 prevChildrenEvts0 children0 =
    case (prevChildren0, prevChildrenEvts0, children0) of
      -- TODO consider ref equality for HTML
      (V.CRawHtml{}, _:_, _) -> fail "patchDomChildren: got overlay for html node!"
      (V.CRawHtml prevHtml, [], V.CRawHtml html) | prevHtml == html -> return []
      (V.CKeyed prevKeyedChildren, evts, V.CKeyed keyedChildren) ->
        patchKeyedDom (DOM.toNode container) prevKeyedChildren evts keyedChildren
      (V.CNormal prevNChildren, evts, V.CNormal nchildren) ->
        patchDom (DOM.toNode container) prevNChildren evts nchildren
      (prevChildren, evts, children) -> do
        removeDomNodeChildren (DOM.toNode container) evts prevChildren
        renderDomChildren container children

  -- | Note that the elements are already assumed to be of the same tag.
  {-# INLINE patchDomElement #-}
  patchDomElement :: forall el1 el2.
       (DOM.IsElement el1, DOM.IsElement el2, DOM.IsElementCSSInlineStyle el2)
    => el2 -- ^ node we're patching
    -> (DOM.JSVal -> el2)
    -> V.Element el1
    -> NodeOverlay
    -> V.Element el2
    -> DOM.JSM NodeOverlay
  patchDomElement node wrap prevEl prevElOverlay el = do
    -- reset properties that are gone to their default
    let removedProps = V.elementProperties prevEl `HMS.difference` V.elementProperties el
    -- TODO we could remove the default properties at this point, but i'm
    -- not sure it's even worth it
    forM_ (HMS.keys removedProps) $ \prop ->
      resetProperty (noResetProperties prevElOverlay HMS.! prop) node
    -- insert new properties, and augment the property resetters
    newResetProperties <- addProperties node wrap (V.elementProperties el) (noResetProperties prevElOverlay)
    -- remove all events
    -- TODO we should probably have an option to be able to have stable events, so
    -- that we do not have to delete everything each time
    -- TODO can it be that changing the attributes triggers some events? we should
    -- check
    forM_ (noEvents prevElOverlay) $ \(SomeSaferEventListener evtName evt) -> do
      -- TODO maybe do something safer here...
      DOM.removeListener (unsafeCoerce node) evtName evt False
      DOM.releaseListener evt
    -- add style
    newStyle <- addStyle node (V.elementStyle el) (noStyle prevElOverlay)
    -- add all events
    evts <- addEvents node (V.elementEvents el)
    -- patch children
    childrenEvts <-
      patchDomChildren node (V.elementChildren prevEl) (noChildren prevElOverlay) (V.elementChildren el)
    return (NodeOverlay evts newResetProperties newStyle childrenEvts (noNode prevElOverlay))

  {-# INLINE patchDomNode #-}
  patchDomNode ::
       DOM.Node -- ^ the container
    -> V.SomeNode -- ^ the previous vdom
    -> NodeOverlay -- ^ the previous vdom events
    -> V.SomeNode -- ^ the next vdom
    -> DOM.JSM NodeOverlay
  patchDomNode container prevVdom@(V.SomeNode (V.Node prevMark prevBody _ _)) prevVdomEvents vdom@(V.SomeNode (V.Node mark body callbacks wrap)) = do
    -- check if they're both marked and without the rerender
    -- also note that if we switch from marked to unmarked, or
    -- if the fingerprints differ, we always assume they're incompatible,
    -- to speed up deletion (especially on jssaddle)
    case (prevMark, mark) of
      (Just (V.Mark prevFprint _), Just (V.Mark fprint rerender)) ->
        if prevFprint == fprint
          then case rerender of
            V.UnsafeDontRerender -> return prevVdomEvents
            V.Rerender -> patch
          else incompatible
      (Just{}, Nothing) -> incompatible
      (Nothing, Just{}) -> incompatible
      (Nothing, Nothing) -> patch
    where
      patch = case (prevBody, body) of
        -- Text
        -- TODO consider ref. equality, also for rawnode
        -- TODO consider asserting no events when appropriate
        (V.NBText prevTxt, V.NBText txt) | prevTxt == txt ->
          return prevVdomEvents
        -- Element
        (V.NBElement prevElement, V.NBElement element) | V.elementTag prevElement == V.elementTag element -> do
          node' <- DOM.unsafeCastTo wrap (noNode prevVdomEvents)
          V.callbacksUnsafeWillPatch callbacks node'
          x <- patchDomElement node' wrap prevElement prevVdomEvents element
          V.callbacksUnsafeDidPatch callbacks node'
          return x
        -- In all other cases we erase and rerender
        _ ->
          incompatible

      incompatible = do
        -- remove all chidren and swap the topmost node
        renderDomNode vdom $ \el evts -> do
          removeNode container (\cont -> DOM.replaceChild_ cont el) prevVdomEvents prevVdom
          return evts

  {-# INLINE patchKeyedDom #-}
  patchKeyedDom ::
       DOM.Node
    -> V.KeyedDom
    -> Overlay
    -> V.KeyedDom
    -> DOM.JSM Overlay
  patchKeyedDom container prevkdom prevVdomEvents kdom = do
    -- TODO implement this properly
    patchDom container (V.unkeyDom prevkdom) prevVdomEvents (V.unkeyDom kdom)

  {-# INLINE patchDom #-}
  patchDom ::
       DOM.Node -- ^ the container
    -> V.Dom -- ^ the previous vdom
    -> Overlay -- ^ the previous vdom events
    -> V.Dom -- ^ the current vdom
    -> DOM.JSM Overlay
  patchDom container prevVnodes0 prevVnodesEvents0 vnodes0 = do
    let
      go prevVnodes prevVdomEvents vnodes = case (prevVnodes, prevVdomEvents, vnodes) of
        ([], _:_, _) -> fail "patchDom: empty prev nodes, but non-empty prev nodes events!"
        (_:_, [], _) -> fail "patchDom: empty prev nodes events, but non-empty prev nodes!"
        ([], [], vnodes') -> renderDom container vnodes'
        (prevVnodes', prevVnodesEvents', []) -> do
          removeDom container prevVnodesEvents'  prevVnodes'
          return mempty
        (prevVnode : prevVnodes', prevVnodeEvents : prevVnodesEvents', node : nodes') -> do
          evt <- patchDomNode container prevVnode prevVnodeEvents node
          evts <- go prevVnodes' prevVnodesEvents' nodes'
          return (evt : evts)
    go (DList.toList prevVnodes0) prevVnodesEvents0 (DList.toList vnodes0)
  in \container mbPrevVdom vdom -> do
    t0 <- liftIO getCurrentTime
    x <- case mbPrevVdom of
      Nothing -> do
        eraseAllChildren container
        renderDom container (DList.toList vdom)
      Just (prevVdom, prevVdomEvents) -> do
        patchDom (DOM.toNode container) prevVdom prevVdomEvents vdom
    t1 <- liftIO getCurrentTime
    logDebug ("Vdom rendered (" <> pack (show (diffUTCTime t1 t0)) <> ")")
    return x
