{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ApplicativeDo #-}
module Anapo.OrderedHashMap
  ( OrderedHashMap
  , fromList
  , getMap
  , getOrder
  , (!)
  , lookup
  , replace
  , unsafeNew
  ) where

import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import Data.Hashable (Hashable)

data OrderedHashMap k v = OrderedHashMap
  { ohmMap :: HMS.HashMap k v
  , ohmOrder :: V.Vector k
  } deriving (Functor, Eq, Show)

instance (Eq k, Hashable k) => Foldable (OrderedHashMap k) where
  {-# INLINE foldr #-}
  foldr f x0 OrderedHashMap{..} = foldr (\k x -> f (ohmMap HMS.! k) x) x0 ohmOrder

instance (Eq k, Hashable k) => Traversable (OrderedHashMap k) where
  {-# INLINE traverse #-}
  traverse f ohm =
    fmap (\kvs -> ohm{ ohmMap = HMS.fromList kvs }) $
      traverse (\k -> (k,) <$> f (ohmMap ohm HMS.! k)) (V.toList (ohmOrder ohm))

{-# INLINE fromList #-}
fromList :: (HasCallStack, Hashable k, Eq k) => [(k, v)] -> OrderedHashMap k v
fromList kvs = if HMS.size ohmMap /= V.length ohmOrder
  then error "duplicate keys!"
  else OrderedHashMap{..}
  where
    ohmMap = HMS.fromList kvs
    ohmOrder = V.fromList (map fst kvs)

{-# INLINE (!) #-}
(!) :: (HasCallStack, Hashable k, Eq k) => OrderedHashMap k v -> k -> v
(!) ohm k = ohmMap ohm HMS.! k

{-# INLINE lookup #-}
lookup :: (Hashable k, Eq k) => OrderedHashMap k v -> k -> Maybe v
lookup ohm k = HMS.lookup k (ohmMap ohm)

{-# INLINE getMap #-}
getMap :: OrderedHashMap k v -> HMS.HashMap k v
getMap OrderedHashMap{..} = ohmMap

{-# INLINE getOrder #-}
getOrder :: OrderedHashMap k v -> V.Vector k
getOrder OrderedHashMap{..} = ohmOrder

{-# INLINE replace #-}
replace :: (HasCallStack, Hashable k, Eq k) => k -> v -> OrderedHashMap k v -> OrderedHashMap k v
replace k v ohm = if HMS.member k (ohmMap ohm)
  then ohm{ ohmMap = HMS.insert k v (ohmMap ohm) }
  else error "key not present!"

{-# INLINE unsafeNew #-}
unsafeNew :: HMS.HashMap k v -> V.Vector k -> OrderedHashMap k v
unsafeNew ohmMap ohmOrder = OrderedHashMap{..}
