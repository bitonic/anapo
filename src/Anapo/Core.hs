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
data VirtualDomNode =
    -- TODO this existential might be detrimental for performance,
    -- consider removing it
    forall el. (DOM.IsElement el) => VDNElement (VirtualDomElement el)
  | VDNText T.Text
  | VDNRawNode DOM.Node
  | VDNMarked Fingerprint Render ~VirtualDomNode
  -- ^ the dom node is lazy here to avoid recomputing it if we don't
  -- end up rerendering

data SomeEvent el = forall e. (DOM.IsEvent e) =>
  SomeEvent (DOM.EventName el e) (el -> e -> ClientM ())

type ElementTag = T.Text
type ElementAttributes = HMS.HashMap T.Text T.Text
type ElementEvents el = [SomeEvent el]

data VirtualDomElement el = VirtualDomElement
  { vdeTag :: ElementTag
  , vdeElement :: DOM.JSVal -> el
  , vdeAttributes :: ElementAttributes
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

