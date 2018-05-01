{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Anapo.VDOM where

import Data.DList (DList)
import GHC.Fingerprint.Type (Fingerprint)
import qualified Data.HashMap.Strict as HMS
import Data.Vector (Vector)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

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

instance Semigroup Rerender where
  Rerender <> _ = Rerender
  _ <> Rerender = Rerender
  _ <> _ = UnsafeDontRerender

instance Monoid Rerender where
  mempty = UnsafeDontRerender
  mappend = (<>)

data SomeVDomNode = forall el. (DOM.IsNode el) => SomeVDomNode (VDomNode el)

{-# INLINE forSomeNodeBody #-}
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
  { callbacksWillMount :: el -> DOM.JSM ()
  , callbacksDidMount :: el -> DOM.JSM ()
  , callbacksWillPatch :: el -> DOM.JSM ()
  , callbacksDidPatch :: el -> DOM.JSM ()
  , callbacksWillRemove :: el -> DOM.JSM ()
  }

instance Monoid (Callbacks el) where
  {-# INLINE mempty #-}
  mempty = Callbacks
    { callbacksWillMount = \_ -> return ()
    , callbacksDidMount = \_ -> return ()
    , callbacksWillPatch = \_ -> return ()
    , callbacksDidPatch = \_ -> return ()
    , callbacksWillRemove = \_ -> return ()
    }
  {-# INLINE mappend #-}
  callbacks1 `mappend` callbacks2 = Callbacks
    { callbacksWillMount = \el -> callbacksWillMount callbacks1 el >> callbacksWillMount callbacks2 el
    , callbacksDidMount = \el -> callbacksDidMount callbacks1 el >> callbacksDidMount callbacks2 el
    , callbacksWillPatch = \el -> callbacksWillPatch callbacks1 el >> callbacksWillPatch callbacks2 el
    , callbacksDidPatch = \el -> callbacksDidPatch callbacks1 el >> callbacksDidPatch callbacks2 el
    , callbacksWillRemove = \el -> callbacksWillRemove callbacks1 el >> callbacksWillRemove callbacks2 el
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
