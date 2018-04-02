{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Anapo.VDOM where

import Data.DList (DList)
import GHC.Fingerprint.Type (Fingerprint)
import qualified Data.HashMap.Strict as HMS
import Data.Vector (Vector)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import Anapo.Text
import Anapo.OrderedHashMap

-- Core types
-- --------------------------------------------------------------------

-- both fields are lazy since when we traverse these we might end up not
-- rendering anything because the 'Mark' stuff.
data Node body = Node
  { nodeBody :: ~body
  , nodeChildren :: ~(Maybe (Children body))
  -- ^ not present if the body does not allow it, e.g. 'VDBRawNode' and
  -- 'VDBText'
  }
  deriving (Functor, Foldable, Traversable)

data Children body =
    CRawHtml Text
  | CNormal (Vector (Node body))
  | CKeyed (OrderedHashMap Text (Node body))
  | CMap (HMS.HashMap Text (Node body))
  deriving (Functor, Foldable, Traversable)

-- Core types
-- --------------------------------------------------------------------

data Rerender = Rerender | UnsafeDontRerender
  deriving (Eq, Show)

data SomeVDomNode = forall el. (DOM.IsNode el) => SomeVDomNode (VDomNode el)

forSomeNodeBody ::
     Monad m
  => Node SomeVDomNode
  -> (forall el. DOM.IsNode el => VDomNode el -> m (VDomNode el))
  -> m (Node SomeVDomNode)
forSomeNodeBody node f = case nodeBody node of
  SomeVDomNode node' -> do
    node'' <- f node'
    return node{nodeBody = SomeVDomNode node''}

-- Something that will turn in a single DOM node.
data VDomNode el = VDomNode
  { vdomMark :: Maybe Mark
  , vdomBody :: VDomBody el
  , vdomCallbacks :: Callbacks el
  , vdomWrap :: DOM.JSVal -> el
  }

data Mark = Mark
  { markFingerprint :: Fingerprint
  , markRerender :: Rerender
  }

data Callbacks el = Callbacks
  { callbacksUnsafeWillMount :: el -> DOM.JSM ()
  , callbacksUnsafeDidMount :: el -> DOM.JSM ()
  , callbacksUnsafeWillPatch :: el -> DOM.JSM ()
  , callbacksUnsafeDidPatch :: el -> DOM.JSM ()
  , callbacksUnsafeWillRemove :: el -> DOM.JSM ()
  }

instance Monoid (Callbacks el) where
  {-# INLINE mempty #-}
  mempty = Callbacks
    { callbacksUnsafeWillMount = \_ -> return ()
    , callbacksUnsafeDidMount = \_ -> return ()
    , callbacksUnsafeWillPatch = \_ -> return ()
    , callbacksUnsafeDidPatch = \_ -> return ()
    , callbacksUnsafeWillRemove = \_ -> return ()
    }
  {-# INLINE mappend #-}
  callbacks1 `mappend` callbacks2 = Callbacks
    { callbacksUnsafeWillMount = \el -> callbacksUnsafeWillMount callbacks1 el >> callbacksUnsafeWillMount callbacks2 el
    , callbacksUnsafeDidMount = \el -> callbacksUnsafeDidMount callbacks1 el >> callbacksUnsafeDidMount callbacks2 el
    , callbacksUnsafeWillPatch = \el -> callbacksUnsafeWillPatch callbacks1 el >> callbacksUnsafeWillPatch callbacks2 el
    , callbacksUnsafeDidPatch = \el -> callbacksUnsafeDidPatch callbacks1 el >> callbacksUnsafeDidPatch callbacks2 el
    , callbacksUnsafeWillRemove = \el -> callbacksUnsafeWillRemove callbacks1 el >> callbacksUnsafeWillRemove callbacks2 el
    }

data VDomBody el where
  VDBElement :: (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => Element el -> VDomBody el
  VDBText :: Text -> VDomBody DOM.Text
  VDBRawNode :: el -> VDomBody el
  -- NOTE: When using 'NBRawNode', make sure to _not_ remove / replace
  -- it, otherwise anapo will break. If you have foreign code manipulate
  -- it create a container node and then pass a child node to the
  -- foreign code.

data SomeEvent el = forall e. (DOM.IsEvent e) =>
  SomeEvent (DOM.EventName el e) (el -> e -> DOM.JSM ())

type ElementTag = Text
type ElementPropertyName = Text
type ElementProperty = DOM.JSVal
type ElementProperties = HMS.HashMap ElementPropertyName ElementProperty
type ElementEvents el = DList (SomeEvent el)
type StylePropertyName = Text
type StyleProperty = Text
type ElementStyle = HMS.HashMap StylePropertyName StyleProperty
type AttributeName = Text
type AttributeBody = DOM.JSVal
type ElementAttributes = HMS.HashMap AttributeName AttributeBody

data Element el = Element
  { elementTag :: ElementTag
  , elementProperties :: ElementProperties
  , elementStyle :: ElementStyle
  , elementAttributes :: ElementAttributes
  , elementEvents :: ElementEvents el
  }
