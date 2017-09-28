module Anapo.VDOM where

import qualified Data.HashMap.Strict as HMS
import Data.DList (DList)
import Data.Typeable.Internal (Fingerprint)
import Data.JSString (JSString)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import Anapo.ClientM
import Anapo.Orphans ()

-- Core types
-- --------------------------------------------------------------------

type Dom = DList SomeNode

data Rerender = Rerender | UnsafeDontRerender
  deriving (Eq, Show)

data SomeNode = forall el. (DOM.IsNode el) => SomeNode (Node el)

-- Something that will turn in a single DOM node.
data Node el = Node
  { nodeMark :: Maybe Mark
  , nodeBody :: ~(NodeBody el)
  -- ^ the dom node is lazy here to avoid recomputing it if we don't
  -- end up rerendering
  , nodeCallbacks :: Callbacks el
  , nodeWrap :: DOM.JSVal -> el
  }

data Mark = Mark
  { markFingerprint :: Fingerprint
  , markRerender :: Rerender
  }

data Callbacks el = Callbacks
  { callbacksUnsafeWillMount :: el -> ClientM ()
  , callbacksUnsafeDidMount :: el -> ClientM ()
  , callbacksUnsafeWillPatch :: el -> ClientM ()
  , callbacksUnsafeDidPatch :: el -> ClientM ()
  , callbacksUnsafeWillRemove :: el -> ClientM ()
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

data NodeBody el where
  NBElement :: (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => Element el -> NodeBody el
  NBText :: JSString -> NodeBody DOM.Text
  NBRawNode :: el -> NodeBody el

data SomeEvent el = forall e. (DOM.IsEvent e) =>
  SomeEvent (DOM.EventName el e) (el -> e -> ClientM ())

data ElementProperty el = forall a. ElementProperty
  { eaGetProperty :: el -> ClientM a
  , eaSetProperty :: el -> a -> ClientM ()
  , eaValue :: a
  }

type ElementTag = JSString
type ElementPropertyName = JSString
type ElementProperties el = HMS.HashMap ElementPropertyName (ElementProperty el)
type ElementEvents el = [SomeEvent el]
type StylePropertyName = JSString
type StyleProperty = JSString
type ElementStyle = HMS.HashMap StylePropertyName StyleProperty

data Element el = Element
  { elementTag :: ElementTag
  , elementProperties :: ElementProperties el
  , elementStyle :: ElementStyle
  , elementEvents :: ElementEvents el
  , elementChildren :: Children
  }

newtype KeyedDom = KeyedDom (DList (JSString, SomeNode))
  deriving (Monoid)

{-# INLINE unkeyDom #-}
unkeyDom :: KeyedDom -> Dom
unkeyDom (KeyedDom kdom) = fmap snd kdom

-- | Things that can be grouped under a node:
-- * a list of nodes;
-- * a list of keyed nodes;
-- * some raw html.
data Children =
    CRawHtml JSString
  | CKeyed KeyedDom
  | CNormal Dom

