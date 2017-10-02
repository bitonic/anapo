{-# LANGUAGE OverloadedStrings #-}
module Anapo.Render (RenderOptions(..), Overlay, renderVirtualDom) where

import qualified Data.HashMap.Strict as HMS
import Control.Monad (forM_, when, forM)
import qualified Data.DList as DList
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_, for_)
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
  }

emptyOverlay :: NodeOverlay
emptyOverlay = NodeOverlay mempty mempty mempty mempty

data RenderOptions = RenderOptions
  { roAlwaysRerender :: Bool
  }

renderVirtualDom :: forall el0.
     (DOM.IsElement el0)
  => RenderOptions -> DOM.Document -> el0 -> Maybe (V.Dom, Overlay) -> V.Dom
  -> DOM.JSM Overlay
renderVirtualDom RenderOptions{..} doc = let
  {-# INLINE removeAllChildren #-}
  removeAllChildren :: (DOM.IsNode el) => el -> DOM.JSM ()
  removeAllChildren el = let
    go = do
      mbChild <- DOM.getFirstChild el
      forM_ mbChild $ \child -> do
        DOM.removeChild_ el child
        go
    in go

  {-# INLINE removeDomNodeChildren #-}
  removeDomNodeChildren ::
       (DOM.IsNode el)
    => el -- ^ the node
    -> Overlay
    -> V.Children
    -> DOM.JSM ()
  removeDomNodeChildren domNode overlay children = case children of
    V.CRawHtml{} -> if null overlay
      then return ()
      else fail "removeDomNodeChildren: expecting no overlay nodes with rawhtml, but got some"
    V.CKeyed kvd -> removeDom domNode Nothing overlay (DList.toList (V.unkeyDom kvd))
    V.CNormal vdom -> removeDom domNode Nothing overlay (DList.toList vdom)

  {-# INLINE removeDomNode #-}
  removeDomNode ::
       (DOM.IsNode el)
    => el -- ^ the node
    -> NodeOverlay
    -> V.SomeNode -- ^ the virtual dom nodes describing what's the node
    -> DOM.JSM ()
  removeDomNode domNode NodeOverlay{..} (V.SomeNode node@V.Node{..}) = do
    V.callbacksUnsafeWillRemove nodeCallbacks =<< DOM.unsafeCastTo (V.nodeWrap node) domNode
    case nodeBody of
      V.NBText{} -> return ()
      V.NBRawNode{} -> return ()
      V.NBElement V.Element{V.elementChildren} -> do
        removeDomNodeChildren domNode noChildren elementChildren
        traverse_ (\(SomeSaferEventListener _ ev) -> DOM.releaseListener ev) noEvents

  {-# INLINE removeDom #-}
  removeDom ::
       (DOM.IsNode el1)
    => el1
    -- ^ the container node
    -> Maybe DOM.Node
    -- ^ the node to start removing from. if 'Nothing' will start from
    -- first child of container
    -> Overlay
    -> [V.SomeNode] -- ^ the virtual dom nodes describing what's in the node
    -> DOM.JSM ()
  removeDom container mbCursor00 overlay00 nodes00 = do
    let
      go :: Maybe DOM.Node -> Overlay -> [V.SomeNode] -> DOM.JSM ()
      go mbCursor overlay0 nodes0 = case (mbCursor, overlay0, nodes0) of
        (Just{}, _, []) ->
          fail  "removeDom: Expecting no cursor at the end of vdom, but got one!"
        (Nothing, _:_, []) ->
          fail  "removeDom: Expecting no overlay at the end of vdom, but got one!"
        (Nothing, [], []) -> return ()
        (Nothing, _, _:_) ->
          fail "removeDom: Expecting cursor since I've still got vdom nodes, but got none!"
        (Just{}, [], _:_) ->
          fail "removeDom: Expeting overlay since i've still got vdom nodes, but got none"
        (Just cursor, overlayNode : overlay, node : nodes) -> do
          nextCursor <- DOM.getNextSibling cursor
          removeDomNode cursor overlayNode node
          DOM.removeChild_ container cursor
          go nextCursor overlay nodes
    mbCursor <- case mbCursor00 of
      Nothing -> DOM.getFirstChild container
      Just cursor -> return (Just cursor)
    go mbCursor overlay00 nodes00

  {-# INLINE addEvents #-}
  addEvents ::
       (DOM.IsElement el)
    => el
    -> V.ElementEvents el
    -> DOM.JSM [SomeSaferEventListener]
  addEvents el evts = forM evts $ \(V.SomeEvent evtName evt) -> do
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
        cont txtNode nodeCallbacks emptyOverlay
      V.NBRawNode el -> cont el nodeCallbacks emptyOverlay
      V.NBElement V.Element{..} -> do
        el <- DOM.unsafeCastTo nodeWrap =<< DOM.createElement doc elementTag
        defProps <- addProperties el nodeWrap elementProperties mempty
        style <- addStyle el elementStyle mempty
        evts <- addEvents el elementEvents
        childrenEvents <- renderDomChildren el elementChildren
        cont el nodeCallbacks (NodeOverlay evts defProps style childrenEvents)

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
        patchKeyedDom container prevKeyedChildren evts keyedChildren
      (V.CNormal prevNChildren, evts, V.CNormal nchildren) ->
        patchDom container prevNChildren evts nchildren
      (prevChildren, evts, children) -> do
        removeDomNodeChildren container evts prevChildren
        renderDomChildren container children

  -- | Note that the elements are already assumed to be of the same
  -- tag.
  {-# INLINE patchDomElement #-}
  patchDomElement :: forall el1 el2.
       (DOM.IsElement el1, DOM.IsElement el2, DOM.IsElementCSSInlineStyle el2)
    => el2 -- ^ the node we're patching
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
    return (NodeOverlay evts newResetProperties newStyle childrenEvts)

  {-# INLINE patchDomNode #-}
  patchDomNode ::
       (DOM.IsNode el1, DOM.IsNode el2)
    => el1 -- ^ the container
    -> el2 -- ^ the dom node we're patching
    -> V.SomeNode -- ^ the previous vdom
    -> NodeOverlay -- ^ the previous vdom events
    -> V.SomeNode -- ^ the next vdom
    -> DOM.JSM NodeOverlay
  patchDomNode container node prevVdom@(V.SomeNode (V.Node prevMark prevBody _ _)) prevVdomEvents vdom@(V.SomeNode (V.Node mark body callbacks wrap)) = do
    -- check if they're both marked and without the rerender
    case (prevMark, mark) of
      (Just (V.Mark prevFprint _), Just (V.Mark fprint V.UnsafeDontRerender)) | prevFprint == fprint -> return prevVdomEvents
      _ -> case (prevBody, body) of
        -- Text
        -- TODO consider ref. equality, also for rawnode
        -- TODO consider asserting no events when appropriate
        (V.NBText prevTxt, V.NBText txt) | prevTxt == txt -> return emptyOverlay
        -- Element
        (V.NBElement prevElement, V.NBElement element) | V.elementTag prevElement == V.elementTag element -> do
          node' <- DOM.unsafeCastTo wrap node
          V.callbacksUnsafeWillPatch callbacks node'
          x <- patchDomElement node' wrap prevElement prevVdomEvents element
          V.callbacksUnsafeDidPatch callbacks node'
          return x
        -- In all other cases we erase and rerender
        _ -> do
          removeDomNode node prevVdomEvents prevVdom
          renderDomNode vdom $ \el evts -> do
            DOM.replaceChild_ container el node
            return evts

  {-# INLINE patchKeyedDom #-}
  patchKeyedDom ::
       (DOM.IsNode el)
    => el -- ^ the container
    -> V.KeyedDom
    -> Overlay
    -> V.KeyedDom
    -> DOM.JSM Overlay
  patchKeyedDom container prevkdom prevVdomEvents kdom = do
    -- TODO implement this properly
    patchDom container (V.unkeyDom prevkdom) prevVdomEvents (V.unkeyDom kdom)

  {-# INLINE patchDom #-}
  patchDom ::
       (DOM.IsNode el)
    => el -- ^ the container
    -> V.Dom -- ^ the previous vdom
    -> Overlay -- ^ the previous vdom events
    -> V.Dom -- ^ the current vdom
    -> DOM.JSM Overlay
  patchDom container prevVnodes0 prevVnodesEvents0 vnodes0 = do
    let
      go mbCursor prevVnodes prevVdomEvents vnodes = case (prevVnodes, prevVdomEvents, vnodes) of
        ([], _:_, _) -> fail "patchDom: empty prev nodes, but non-empty prev nodes events!"
        (_:_, [], _) -> fail "patchDom: empty prev nodes events, but non-empty prev nodes!"
        ([], [], vnodes') -> do
          forM_ mbCursor $ \_ ->
            fail "patchDom: expecting no cursor at the end of previous nodes, but got one"
          renderDom container vnodes'
        (prevVnodes', prevVnodesEvents', []) -> do
          removeDom container mbCursor prevVnodesEvents'  prevVnodes'
          return mempty
        (prevVnode : prevVnodes', prevVnodeEvents : prevVnodesEvents', node : nodes') -> do
          case mbCursor of
            Nothing -> do
              fail "patchDom: expecting cursor since I have remaining previous nodes, but got none"
            Just cursor -> do
              nextCursor <- DOM.getNextSibling cursor
              evt <- patchDomNode container cursor prevVnode prevVnodeEvents node
              evts <- go nextCursor prevVnodes' prevVnodesEvents' nodes'
              return (evt : evts)
    mbCursor <- DOM.getFirstChild container
    go mbCursor (DList.toList prevVnodes0) prevVnodesEvents0 (DList.toList vnodes0)

  in \container mbPrevVdom vdom -> do
    t0 <- liftIO getCurrentTime
    x <- case mbPrevVdom of
      Nothing -> do
        removeAllChildren container
        renderDom container (DList.toList vdom)
      Just (prevVdom, prevVdomEvents) -> if roAlwaysRerender
        then do
          removeDom container Nothing prevVdomEvents (DList.toList prevVdom)
          renderDom container (DList.toList vdom)
        else do
          patchDom container prevVdom prevVdomEvents vdom
    t1 <- liftIO getCurrentTime
    logDebug ("Vdom rendered (" <> pack (show (diffUTCTime t1 t0)) <> ")")
    return x
