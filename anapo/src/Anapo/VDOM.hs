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

-- Something that will turn in a single DOM node.
data VDomNode el = VDomNode
  { vdomMark :: Maybe Mark
  , vdomBody :: VDomBody el
  , vdomCallbacks :: Callbacks
  , vdomWrap :: DOM.JSVal -> el
  }

data Mark = Mark
  { markFingerprint :: Fingerprint
  , markRerender :: Rerender
  }

data Callbacks = Callbacks
  { callbacksUnsafeWillMount :: DOM.Node -> DOM.JSM ()
  , callbacksUnsafeDidMount :: DOM.Node -> DOM.JSM ()
  , callbacksUnsafeWillPatch :: DOM.Node -> DOM.JSM ()
  , callbacksUnsafeDidPatch :: DOM.Node -> DOM.JSM ()
  , callbacksUnsafeWillRemove :: DOM.Node -> DOM.JSM ()
  }

instance Monoid Callbacks where
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

data ElementProperty el = forall a. ElementProperty
  { eaGetProperty :: el -> DOM.JSM a
  , eaSetProperty :: el -> a -> DOM.JSM ()
  , eaValue :: DOM.JSM a
  }

type ElementTag = Text
type ElementPropertyName = Text
type ElementProperties el = HMS.HashMap ElementPropertyName (ElementProperty el)
type ElementEvents el = DList (SomeEvent el)
type StylePropertyName = Text
type StyleProperty = Text
type ElementStyle = HMS.HashMap StylePropertyName StyleProperty

data Element el = Element
  { elementTag :: ElementTag
  , elementProperties :: ElementProperties el
  , elementStyle :: ElementStyle
  , elementEvents :: ElementEvents el
  }
