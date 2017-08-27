module Anapo.Render (RenderOptions(..), VirtualDomOverlay, renderVirtualDom) where

import qualified Data.HashMap.Strict as HMS
import Control.Monad (forM_, when, forM)
import qualified Data.DList as DList
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (catMaybes)

import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventTargetClosures as DOM
import qualified GHCJS.DOM.EventTarget as DOM

import Anapo.Core

type VirtualDomOverlay = [VirtualDomNodeOverlay]

newtype ResetProperty = ResetProperty
  { resetProperty :: forall el. (DOM.IsElement el) => el -> ClientM ()
  }

data VirtualDomNodeOverlay = VirtualDomNodeOverlay
  { vdnoEvents :: [(DOM.DOMString, DOM.EventListener)]
  , vdnoResetProperties :: HMS.HashMap ElementPropertyName ResetProperty
  -- ^ holds functions that can reset all the properties that
  -- have been set.
  , vdnoChildren :: VirtualDomOverlay
  }

emptyOverlay :: VirtualDomNodeOverlay
emptyOverlay = VirtualDomNodeOverlay mempty mempty mempty

data RenderOptions = RenderOptions
  { roAlwaysRerender :: Bool
  , roDebugOutput :: Bool
  }

renderVirtualDom :: forall el0.
     (DOM.IsElement el0)
  => RenderOptions -> DOM.Document -> el0 -> Maybe (VirtualDom, VirtualDomOverlay) -> VirtualDom
  -> ClientM VirtualDomOverlay
renderVirtualDom RenderOptions{..} doc = let
  {-# INLINE removeAllChildren #-}
  removeAllChildren :: (DOM.IsNode el) => el -> ClientM ()
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
    -> VirtualDomChildren
    -> ClientM ()
  removeDomNodeChildren domNode = \case
    VDCRawHtml{} -> return ()
    VDCKeyed kvd -> removeDom domNode Nothing (DList.toList (unkeyedVirtualDom kvd))
    VDCNormal vdom -> removeDom domNode Nothing (DList.toList vdom)

  {-# INLINE removeDomNode #-}
  removeDomNode ::
       (DOM.IsNode el)
    => el -- ^ the node
    -> VirtualDomNode -- ^ the virtual dom nodes describing what's the node
    -> ClientM ()
  removeDomNode domNode VirtualDomNode{..} = do
    vdncUnsafeWillRemove vdnCallbacks =<< DOM.unsafeCastTo vdnWrap domNode
    case vdnBody of
      VDNBText{} -> return ()
      VDNBRawNode{} -> return ()
      VDNBElement VirtualDomElement{vdeChildren} -> removeDomNodeChildren domNode vdeChildren

  {-# INLINE removeDom #-}
  removeDom ::
       (DOM.IsNode el1)
    => el1
    -- ^ the container node
    -> Maybe DOM.Node
    -- ^ the node to start removing from. if 'Nothing' will start from
    -- first child of container
    -> [VirtualDomNode] -- ^ the virtual dom nodes describing what's in the node
    -> ClientM ()
  removeDom container mbCursor0 nodes0 = do
    let
      go :: Maybe DOM.Node -> [VirtualDomNode] -> ClientM ()
      go mbCursor = \case
        [] -> forM_ mbCursor $ \_ ->
          fail "removeDom: Expecting no cursor at the end of vdom, but got one!"
        node : nodes -> case mbCursor of
          Nothing ->
            fail "removeDom: Expecting cursor since I've still got vdom nodes, but got none!"
          Just cursor -> do
            nextCursor <- DOM.getNextSibling cursor
            removeDomNode cursor node
            DOM.removeChild_ container cursor
            go nextCursor nodes
    mbCursor <- case mbCursor0 of
      Nothing -> DOM.getFirstChild container
      Just cursor -> return (Just cursor)
    go mbCursor nodes0

  {-# INLINE addEvents #-}
  addEvents ::
       (DOM.IsElement el)
    => el
    -> ElementEvents el
    -> ClientM [(DOM.DOMString, DOM.EventListener)]
  addEvents el evts = forM evts $ \(SomeEvent (DOM.EventName evtName) evt) -> do
    safel <- DOM.eventListenerNew (evt el)
    raw <- DOM.EventListener <$> DOM.toJSVal safel
    DOM.addEventListener el evtName (Just raw) False
    return (evtName, raw)

  {-# INLINE addProperties #-}
  addProperties ::
       (DOM.IsElement el)
    => el
    -> (DOM.JSVal -> el)
    -> ElementProperties el
    -> HMS.HashMap ElementPropertyName ResetProperty -- ^ previous reset properties
    -> ClientM (HMS.HashMap ElementPropertyName ResetProperty)
  addProperties el wrap props prevReset =
    fmap (HMS.union prevReset . HMS.fromList . catMaybes) $
      forM (HMS.toList props) $ \(propName, ElementProperty{..}) -> do
        def <- eaGetProperty el
        eaSetProperty el eaValue
        return $ case HMS.lookup propName prevReset of
          Nothing -> Just
            ( propName
            , ResetProperty $ \el' -> do
                el'' <- DOM.unsafeCastTo wrap el'
                eaSetProperty el'' def
            )
          Just{} -> Nothing

  {-# INLINE renderDomChildren #-}
  renderDomChildren ::
       (DOM.IsElement el)
    => el -- ^ container
    -> VirtualDomChildren
    -> ClientM VirtualDomOverlay
  renderDomChildren container = \case
    VDCRawHtml html -> do
      DOM.setInnerHTML container html
      return mempty
    VDCKeyed kvd -> renderDom container (DList.toList (unkeyedVirtualDom kvd))
    VDCNormal vdom -> renderDom container (DList.toList vdom)

  {-# INLINE renderDomNode #-}
  renderDomNode ::
       VirtualDomNode
    -> (forall el. (DOM.IsNode el) => el -> VirtualDomNodeOverlay -> ClientM a)
    -> ClientM a
  renderDomNode VirtualDomNode{..} cont0 = do
    let cont el callbacks evts = do
          vdncUnsafeWillMount callbacks el
          x <- cont0 el evts
          vdncUnsafeDidMount callbacks el
          return x
    case vdnBody of
      VDNBText txt -> do
        txtNode <- DOM.createTextNode doc txt
        cont txtNode vdnCallbacks emptyOverlay
      VDNBRawNode el -> cont el vdnCallbacks emptyOverlay
      VDNBElement VirtualDomElement{..} -> do
        el <- DOM.unsafeCastTo vdnWrap =<< DOM.createElement doc vdeTag
        defProps <- addProperties el vdnWrap vdeProperties mempty
        evts <- addEvents el vdeEvents
        childrenEvents <- renderDomChildren el vdeChildren
        cont el vdnCallbacks (VirtualDomNodeOverlay evts defProps childrenEvents)

  {-# INLINE renderDom #-}
  renderDom ::
       (DOM.IsNode el)
    => el -- ^ the node that should contain the dom
    -> [VirtualDomNode]
    -> ClientM [VirtualDomNodeOverlay]
  renderDom container = mapM $ \node@VirtualDomNode{..} -> do
    renderDomNode node $ \el evts -> do
      DOM.appendChild_ container el
      return evts

  {-# INLINE patchDomChildren #-}
  patchDomChildren ::
       (DOM.IsElement el)
    => el -- ^ the containing node
    -> VirtualDomChildren -- ^ previous children
    -> VirtualDomOverlay -- ^ the overlay for previous children
    -> VirtualDomChildren -- ^ current children
    -> ClientM VirtualDomOverlay
  patchDomChildren container prevChildren0 prevChildrenEvts0 children0 =
    case (prevChildren0, prevChildrenEvts0, children0) of
      -- TODO consider ref equality for HTML
      (VDCRawHtml{}, _:_, _) -> fail "patchDomChildren: got overlay for html node!"
      (VDCRawHtml prevHtml, [], VDCRawHtml html) | prevHtml == html -> return []
      (VDCKeyed prevKeyedChildren, evts, VDCKeyed keyedChildren) ->
        patchKeyedDom container prevKeyedChildren evts keyedChildren
      (VDCNormal prevNChildren, evts, VDCNormal nchildren) ->
        patchDom container prevNChildren evts nchildren
      (prevChildren, _, children) -> do
        -- TODO remove this, it's just a check for now
        removeDomNodeChildren container prevChildren
        renderDomChildren container children

  -- | Note that the elements are already assumed to be of the same
  -- tag.
  {-# INLINE patchDomElement #-}
  patchDomElement :: forall el1 el2.
       (DOM.IsElement el1, DOM.IsElement el2)
    => el2 -- ^ the node we're patching
    -> (DOM.JSVal -> el2)
    -> VirtualDomElement el1
    -> VirtualDomNodeOverlay
    -> VirtualDomElement el2
    -> ClientM VirtualDomNodeOverlay
  patchDomElement node wrap prevEl prevElOverlay el = do
    -- reset properties that are gone to their default
    let removedProps = vdeProperties prevEl `HMS.difference` vdeProperties el
    -- TODO we could remove the default properties at this point, but i'm
    -- not sure it's even worth it
    forM_ (HMS.keys removedProps) $ \prop ->
      resetProperty (vdnoResetProperties prevElOverlay HMS.! prop) node
    -- insert new properties, and augment the property resetters
    newResetProperties <- addProperties node wrap (vdeProperties el) (vdnoResetProperties prevElOverlay)
    -- remove all events
    -- TODO we should probably have an option to be able to have stable events, so
    -- that we do not have to delete everything each time
    -- TODO can it be that changing the attributes triggers some events? we should
    -- check
    forM_ (vdnoEvents prevElOverlay) $ \(evtName, evt) -> do
      DOM.removeEventListener node evtName (Just evt) False
    -- add all events
    evts <- addEvents node (vdeEvents el)
    -- patch children
    childrenEvts <-
      patchDomChildren node (vdeChildren prevEl) (vdnoChildren prevElOverlay) (vdeChildren el)
    return (VirtualDomNodeOverlay evts newResetProperties childrenEvts)

  {-# INLINE patchDomNode #-}
  patchDomNode ::
       (DOM.IsNode el1, DOM.IsNode el2)
    => el1 -- ^ the container
    -> el2 -- ^ the dom node we're patching
    -> VirtualDomNode -- ^ the previous vdom
    -> VirtualDomNodeOverlay -- ^ the previous vdom events
    -> VirtualDomNode -- ^ the next vdom
    -> ClientM VirtualDomNodeOverlay
  patchDomNode container node prevVdom@(VirtualDomNode prevMark prevBody _ _) prevVdomEvents vdom@(VirtualDomNode mark body wrap callbacks) = do
    -- check if they're both marked and without the rerender
    case (prevMark, mark) of
      (Just (VirtualDomNodeMark prevFprint _), Just (VirtualDomNodeMark fprint render))
        | prevFprint == fprint && render == DontRender -> return prevVdomEvents
      _ -> case (prevBody, body) of
        -- Text
        -- TODO consider ref. equality, also for rawnode
        -- TODO consider asserting no events when appropriate
        (VDNBText prevTxt, VDNBText txt) | prevTxt == txt -> return emptyOverlay
        -- Element
        (VDNBElement prevElement, VDNBElement element) | vdeTag prevElement == vdeTag element -> do
          node' <- DOM.unsafeCastTo wrap node
          vdncUnsafeWillPatch callbacks node'
          x <- patchDomElement node' wrap prevElement prevVdomEvents element
          vdncUnsafeDidPatch callbacks node'
          return x
        -- In all other cases we erase and rerender
        _ -> do
          removeDomNode node prevVdom
          renderDomNode vdom $ \el evts -> do
            DOM.replaceChild_ container el node
            return evts

  {-# INLINE patchKeyedDom #-}
  patchKeyedDom ::
       (DOM.IsNode el)
    => el -- ^ the container
    -> KeyedVirtualDom
    -> VirtualDomOverlay
    -> KeyedVirtualDom
    -> ClientM VirtualDomOverlay
  patchKeyedDom container (KeyedVirtualDom prevKeyedVdom prevVdomOrder) prevVdomEvents (KeyedVirtualDom keyedVdom vdomOrder) = do
    -- TODO implement this properly
    let prevVdom = fmap
          (\key -> prevKeyedVdom HMS.! key)
          prevVdomOrder
    let vdom = fmap
          (\key -> keyedVdom HMS.! key)
          vdomOrder
    patchDom container prevVdom prevVdomEvents vdom

  {-# INLINE patchDom #-}
  patchDom ::
       (DOM.IsNode el)
    => el -- ^ the container
    -> VirtualDom -- ^ the previous vdom
    -> VirtualDomOverlay -- ^ the previous vdom events
    -> VirtualDom -- ^ the current vdom
    -> ClientM VirtualDomOverlay
  patchDom container prevVnodes0 prevVnodesEvents0 vnodes0 = do
    let
      go mbCursor prevVnodes prevVdomEvents vnodes = case (prevVnodes, prevVdomEvents, vnodes) of
        ([], _:_, _) -> fail "patchDom: empty prev nodes, but non-empty prev nodes events!"
        (_:_, [], _) -> fail "patchDom: empty prev nodes events, but non-empty prev nodes!"
        ([], [], vnodes') -> do
          forM_ mbCursor $ \_ ->
            fail "patchDom: expecting no cursor at the end of previous nodes, but got one"
          renderDom container vnodes'
        (prevVnodes', _, []) -> do
          removeDom container mbCursor prevVnodes'
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

  debugOutput s = liftIO (when roDebugOutput (putStrLn s))

  in \container mbPrevVdom vdom -> do
    t0 <- liftIO getCurrentTime
    x <- case mbPrevVdom of
      Nothing -> do
        removeAllChildren container
        renderDom container (DList.toList vdom)
      Just (prevVdom, prevVdomEvents) -> if roAlwaysRerender
        then do
          removeDom container Nothing (DList.toList prevVdom)
          renderDom container (DList.toList vdom)
        else do
          patchDom container prevVdom prevVdomEvents vdom
    t1 <- liftIO getCurrentTime
    debugOutput ("Vdom rendered (" ++ show (diffUTCTime t1 t0) ++ ")")
    return x
