{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Note: we use 'Traversal' to keep a cursor to the
-- write end of the state, but really we should use an
-- "affine traversal" which guarantees we have either 0
-- or 1 positions to traverse. See
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>
-- for why affine traversals do not play well with lens.
module Anapo.Component
  ( -- * Re-exports
    V.Rerender(..)

    -- * affine traversals
  , AffineTraversal
  , AffineTraversal'
  , toMaybeOf

    -- * Register / handler
  , RegisterThread
  , HandleException
  , forkRegistered
  , Action(..)
  , ActionEnv(..)
  , dispatch
  , dispatchInside
  , actionFork
  , actionZoom
  , actionComponent
  , MonadAction(..)

    -- * AnapoM
  , AnapoM
  , Dom
  , Dom'
  , DomState
  , KeyedDom
  , KeyedDom'
  , KeyedDomState
  , MapDom
  , MapDom'
  , MapDomState
  , Node
  , Node'
  , askRegisterThread
  , askHandleException
  , askPreviousState
  , zoomL
  , zoomT
  , UnliftJSM
  , askUnliftJSM
  , unliftJSM
  , actionUnliftJSM

    -- * Component
  , Component
  , componentState
  , componentNode
  , newComponent
  , component
  , componentT
  , componentL

    -- * basic combinators
  , n
  , n'
  , key
  , key'
  , ukey
  , ukey'
  , text
  , rawNode
  , marked

    -- * callbacks
  , willMount
  , didMount
  , willPatch
  , didPatch
  , willRemove

    -- * raw html
  , UnsafeRawHtml(..)

    -- * node patching
  , patchNode
  , NodePatch(..)

    -- * elements
  , IsElementChildren(..)
  , el
  , div_
  , span_
  , a_
  , p_
  , input_
  , form_
  , h1_
  , h2_
  , h4_
  , h5_
  , h6_
  , select_
  , option_
  , button_
  , ul_
  , li_
  , label_
  , iframe_
  , small_
  , pre_
  , code_
  , nav_
  , table_
  , tbody_
  , tr_
  , td_

    -- * attributes
  , SomeEventAction(..)
  , style
  , class_
  , id_
  , type_
  , href_
  , value_
  , selected_
  , checked_
  , disabled_
  , src_
  , placeholder_
  , multiple_
  , for_
  , property
  , rawAttribute
  , attribute
  , onEvent

    -- * events
  , onclick_
  , onchange_
  , onsubmit_
  , oninput_
  , onselect_

    -- * dom re-exports
  , DOM.Event.preventDefault

    -- * simple rendering
  , simpleNode
  , simpleRenderNode
  ) where

import qualified GHCJS.DOM.Event as DOM.Event

import Anapo.Component.Internal
import qualified Anapo.VDOM as V
