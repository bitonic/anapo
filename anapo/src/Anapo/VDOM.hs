{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Anapo.VDOM
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

#if !defined(ghcjs_HOST_OS)
   , Bwd(..)
#endif
  ) where

import Anapo.VDOM.Internal

