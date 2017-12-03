module Anapo.VDOM where

import qualified Data.HashMap.Strict as HMS
import Data.DList (DList)
import GHC.Fingerprint.Type (Fingerprint)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import Anapo.Text

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

data NodeBody el where
  NBElement :: (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => Element el -> NodeBody el
  NBText :: Text -> NodeBody DOM.Text
  NBRawNode :: el -> NodeBody el

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
  , elementChildren :: Children
  }

newtype KeyedDom = KeyedDom (DList (Text, SomeNode))
  deriving (Monoid)

{-# INLINE unkeyDom #-}
unkeyDom :: KeyedDom -> Dom
unkeyDom (KeyedDom kdom) = fmap snd kdom

-- | Things that can be grouped under a node:
-- * a list of nodes;
-- * a list of keyed nodes;
-- * some raw html.
data Children =
    CRawHtml Text
  | CKeyed KeyedDom
  | CNormal Dom

