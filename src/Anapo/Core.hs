module Anapo.Core where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.DList (DList)
import Data.Typeable.Internal (Fingerprint)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

-- How to run the client monad
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)
runClientM :: ClientM () -> IO ()
runClientM = id
#else
import qualified Language.Javascript.JSaddle.Warp as DOM
runClientM :: ClientM () -> IO ()
runClientM cm = do
  putStrLn "Running on port 8000"
  DOM.run 8000 cm
#endif

type ClientM = DOM.JSM

-- Core types
-- --------------------------------------------------------------------

type VirtualDom = DList VirtualDomNode

data Render = ReRender | DontRender
  deriving (Eq, Show)

-- Something that will turn in a single DOM node.
data VirtualDomNode = forall el. (DOM.IsNode el) => VirtualDomNode
  { vdnMark :: Maybe VirtualDomNodeMark
  , vdnBody :: ~(VirtualDomNodeBody el)
  -- ^ the dom node is lazy here to avoid recomputing it if we don't
  -- end up rerendering
  , vdnWrap :: DOM.JSVal -> el
  , vdnCallbacks :: VirtualDomNodeCallbacks el
  }

data VirtualDomNodeCallbacks el = VirtualDomNodeCallbacks
  { vdncUnsafeWillMount :: el -> ClientM ()
  , vdncUnsafeDidMount :: el -> ClientM ()
  , vdncUnsafeWillPatch :: el -> ClientM ()
  , vdncUnsafeDidPatch :: el -> ClientM ()
  , vdncUnsafeWillRemove :: el -> ClientM ()
  }

instance Monoid (VirtualDomNodeCallbacks el) where
  mempty = noVirtualDomNodeCallbacks
  callbacks1 `mappend` callbacks2 = VirtualDomNodeCallbacks
    { vdncUnsafeWillMount = vdncUnsafeWillMount callbacks1 >> vdncUnsafeWillMount callbacks2
    , vdncUnsafeDidMount = vdncUnsafeDidMount callbacks1 >> vdncUnsafeDidMount callbacks2
    , vdncUnsafeWillPatch = vdncUnsafeWillPatch callbacks1 >> vdncUnsafeWillPatch callbacks2
    , vdncUnsafeDidPatch = vdncUnsafeDidPatch callbacks1 >> vdncUnsafeDidPatch callbacks2
    , vdncUnsafeWillRemove = vdncUnsafeWillRemove callbacks1 >> vdncUnsafeWillRemove callbacks2
    }

{-# NOINLINE noVirtualDomNodeCallbacks #-}
noVirtualDomNodeCallbacks :: VirtualDomNodeCallbacks el
noVirtualDomNodeCallbacks = VirtualDomNodeCallbacks
  { vdncUnsafeWillMount = \_ -> return ()
  , vdncUnsafeDidMount = \_ -> return ()
  , vdncUnsafeWillPatch = \_ -> return ()
  , vdncUnsafeDidPatch = \_ -> return ()
  , vdncUnsafeWillRemove = \_ -> return ()
  }

data VirtualDomNodeMark = VirtualDomNodeMark
  { vdnmFingerprint :: Fingerprint
  , vdnmRender :: Render
  }

data VirtualDomNodeBody el where
  VDNBElement :: (DOM.IsElement el) => VirtualDomElement el -> VirtualDomNodeBody el
  VDNBText :: T.Text -> VirtualDomNodeBody DOM.Text
  VDNBRawNode :: el -> VirtualDomNodeBody el

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

data VirtualDomElement el = VirtualDomElement
  { vdeTag :: ElementTag
  , vdeProperties :: ElementProperties el
  , vdeEvents :: ElementEvents el
  , vdeChildren :: VirtualDomChildren
  }

data KeyedVirtualDom = KeyedVirtualDom
  { kvdNodes :: HMS.HashMap T.Text VirtualDomNode
  , kvdOrder :: DList T.Text
  }

{-# INLINE unkeyedVirtualDom #-}
unkeyedVirtualDom :: KeyedVirtualDom -> VirtualDom
unkeyedVirtualDom KeyedVirtualDom{..} = fmap (\k -> kvdNodes HMS.! k) kvdOrder

instance Monoid KeyedVirtualDom where
  {-# INLINE mempty #-}
  mempty = KeyedVirtualDom mempty mempty
  {-# INLINE mappend #-}
  kvd1 `mappend` kvd2 = let
    commonKeys = kvdNodes kvd1 `HMS.intersection` kvdNodes kvd2
    in if HMS.null commonKeys
      then KeyedVirtualDom (kvdNodes kvd1 `mappend` kvdNodes kvd2) (kvdOrder kvd1 `mappend` kvdOrder kvd2)
      else error ("TODO common keys, make error better: " ++ show (HMS.keys commonKeys))

-- | Things that can be grouped under a node:
-- * a list of nodes;
-- * a list of keyed nodes;
-- * some raw html.
data VirtualDomChildren =
    VDCRawHtml T.Text
  | VDCKeyed KeyedVirtualDom
  | VDCNormal VirtualDom

