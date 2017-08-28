module Anapo.VDOM where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.DList (DList)
import Data.Typeable.Internal (Fingerprint)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import Anapo.ClientM

-- Core types
-- --------------------------------------------------------------------

type Dom = DList Node

data Rerender = Rerender | DontRerender
  deriving (Eq, Show)

-- Something that will turn in a single DOM node.
data Node = forall el. (DOM.IsNode el) => Node
  { nodeMark :: Maybe Mark
  , nodeBody :: ~(NodeBody el)
  -- ^ the dom node is lazy here to avoid recomputing it if we don't
  -- end up rerendering
  , nodeWrap :: DOM.JSVal -> el
  , nodeCallbacks :: Callbacks el
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
    { callbacksUnsafeWillMount = callbacksUnsafeWillMount callbacks1 >> callbacksUnsafeWillMount callbacks2
    , callbacksUnsafeDidMount = callbacksUnsafeDidMount callbacks1 >> callbacksUnsafeDidMount callbacks2
    , callbacksUnsafeWillPatch = callbacksUnsafeWillPatch callbacks1 >> callbacksUnsafeWillPatch callbacks2
    , callbacksUnsafeDidPatch = callbacksUnsafeDidPatch callbacks1 >> callbacksUnsafeDidPatch callbacks2
    , callbacksUnsafeWillRemove = callbacksUnsafeWillRemove callbacks1 >> callbacksUnsafeWillRemove callbacks2
    }

data Mark = Mark
  { markFingerprint :: Fingerprint
  , markRerender :: Rerender
  }

data NodeBody el where
  NBElement :: (DOM.IsElement el) => Element el -> NodeBody el
  NBText :: T.Text -> NodeBody DOM.Text
  NBRawNode :: el -> NodeBody el

data SomeEvent el = forall e. (DOM.IsEvent e) =>
  SomeEvent (DOM.EventName el e) (el -> e -> ClientM ())

data ElementProperty el = forall a. ElementProperty
  { eaGetProperty :: el -> ClientM a
  , eaSetProperty :: el -> a -> ClientM ()
  , eaValue :: a
  }

type ElementTag = T.Text
type ElementPropertyName = T.Text
type ElementProperties el = HMS.HashMap ElementPropertyName (ElementProperty el)
type ElementEvents el = [SomeEvent el]

data Element el = Element
  { elementTag :: ElementTag
  , elementProperties :: ElementProperties el
  , elementEvents :: ElementEvents el
  , elementChildren :: Children
  }

data KeyedDom = KeyedDom
  { keyedDomNodes :: HMS.HashMap T.Text Node
  , keyedDomOrder :: DList T.Text
  }

{-# INLINE unkeyDom #-}
unkeyDom :: KeyedDom -> Dom
unkeyDom KeyedDom{..} = fmap (\k -> keyedDomNodes HMS.! k) keyedDomOrder

instance Monoid KeyedDom where
  {-# INLINE mempty #-}
  mempty = KeyedDom mempty mempty
  {-# INLINE mappend #-}
  kvd1 `mappend` kvd2 = let
    commonKeys = keyedDomNodes kvd1 `HMS.intersection` keyedDomNodes kvd2
    in if HMS.null commonKeys
      then KeyedDom (keyedDomNodes kvd1 `mappend` keyedDomNodes kvd2) (keyedDomOrder kvd1 `mappend` keyedDomOrder kvd2)
      else error ("TODO common keys, make error better: " ++ show (HMS.keys commonKeys))

-- | Things that can be grouped under a node:
-- * a list of nodes;
-- * a list of keyed nodes;
-- * some raw html.
data Children =
    CRawHtml T.Text
  | CKeyed KeyedDom
  | CNormal Dom
