{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Note: we use 'Traversal' to keep a cursor to the
-- write end of the state, but really we should use an
-- "affine traversal" which guarantees we have either 0
-- or 1 positions to traverse. See
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>
-- for why affine traversals do not play well with lens.
module Anapo.Component
  ( Rerender(..)

    -- * affine traversals
  , AffineTraversal
  , AffineTraversal'
  , AffineFold
  , toMaybeOf

    -- * Register / handler
  , RegisterThread
  , HandleException
  , forkRegistered
  , Action
  , DispatchM(..)
  , DispatchM'
  , dispatch
  , dispatchRerender
  , actFork
  , actZoom
  , actZoomCtx
  , noContext
  , noState
  , actComponent
  , MonadAction(..)

    -- * AnapoM
  , DomM
  , Dom
  , Dom'
  , DomState
  , KeyedDom
  , KeyedDom'
  , KeyedDomState
  , Node
  , Node'
  , askRegisterThread
  , askHandleException
  , zoomL
  , zoomT
  , zoomCtxF
  , zoomCtxL
  , askContext
  , viewContext
  , UnliftJSM
  , askUnliftJSM
  , unliftJSM
  , actionUnliftJSM

    -- * Component
  , Component
  , ComponentToken
  , compState
  , compNode
  , newNamedComponent
  , newMarkedComponent
  , newComponent
  , newNamedComponent_
  , newMarkedComponent_
  , newComponent_
  , component
  , component_
  , componentT
  , componentL
  , componentT_
  , componentL_
  , initComponent

    -- * basic combinators
  , n
  , n'
  , key
  , key'
  , text

    -- * callbacks
  , willMount
  , didMount
  , willPatch
  , didPatch
  , willRemove

    -- * raw html
  , UnsafeRawHtml(..)

    -- * node patching
  , NodePatch(..)

    -- * elements
  , IsElementChildren(..)
  , el
  , rawNode
  , marked
  , patchElement

    -- * attributes / properties
  , SomeEventAction(..)
  , style
  , rawProperty
  , textProperty
  , boolProperty
  , rawAttribute
  , textAttribute
  , boolAttribute

    -- * dom re-exports
  , DOM.Event.preventDefault

    -- * simple rendering
  , simpleNode
  , simpleRenderNode

    -- * defined elements
  , module Anapo.Component.Elements
    -- * defined attributes
  , module Anapo.Component.Attributes
    -- * defined events
  , module Anapo.Component.Events
  ) where

import qualified GHCJS.DOM.Event as DOM.Event

import Anapo.Component.Internal
import Anapo.Component.Elements
import Anapo.Component.Attributes
import Anapo.Component.Events

