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

    -- * Dispatch
  , Dispatch(..)

    -- * Register / handler
  , RegisterThread
  , HandleException
  , forkRegistered
  , Action(..)
  , ActionEnv(..)
  , dispatch
  , forkAction
  , zoomAction
  , MonadAction(..)

    -- * AnapoM
  , AnapoM
  , Dom
  , Node
  , askRegisterThread
  , askHandleException
  , askPreviousState
  , zoomL
  , zoomT

    -- * basic combinators
  , n
  , key
  , text
  , rawNode
  , marked

    -- * callbacks
  , unsafeWillMount
  , unsafeDidMount
  , unsafeWillPatch
  , unsafeDidPatch
  , unsafeWillRemove

    -- * raw html
  , UnsafeRawHtml(..)

    -- * elements
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
  , multiple_
  , iframe_
  , small_
  , pre_
  , code_
  , nav_

    -- * attributes
  , SomeEventAction(..)
  , style
  , class_
  , id_
  , HasTypeProperty(..)
  , type_
  , HasHrefProperty(..)
  , href_
  , HasValueProperty(..)
  , value_
  , selected_
  , HasCheckedProperty(..)
  , checked_
  , HasDisabledProperty(..)
  , disabled_
  , src_
  , placeholder_
  , for_
  , rawProperty
  , rawAttribute
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

    -- * component
  , Component
  , newComponent
  , component
  , componentT
  , componentL
  ) where

import qualified GHCJS.DOM.Event as DOM.Event

import Anapo.Component.Internal
import qualified Anapo.VDOM as V
