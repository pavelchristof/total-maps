{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Description :  Subset dense total map.
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC >= 7.10
--
-- Dense total maps parametrized by a set of keys.
-----------------------------------------------------------------------------
module Data.Total.Map.Subset (
    Subset,
    TotalSubsetMap(..),
    restrict
    ) where

import           Data.Bytes.Serial
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Key
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection
import qualified Data.Set as Set
import           Data.Total.Subset
import           Linear
import           Prelude hiding (zip)

-- | A total map from a subset s of keys k to values a, e.g. a restriction
-- of a partial function @k -> a@ to a subset of its domain on which the
-- function is defined.
--
-- Most functions are derived from 'Data.Map.Map'.
--
-- n is equal to the size of the key set.
newtype TotalSubsetMap s k a = TotalSubsetMap (Map k a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Zippy applicative. Complexity: 'pure' O(n), '<*>' O(n).
instance (Ord k, Subset s k) => Applicative (TotalSubsetMap s k) where
    pure x = TotalSubsetMap $ Map.fromSet (const x) (reflect (Proxy :: Proxy s))
    (<*>)  = zap

-- Keys instances.

type instance Key (TotalSubsetMap s k) = k

-- TODO: it would be nice to document these, but haddock doesn't allow that.

deriving instance Keyed (TotalSubsetMap s k)
deriving instance Ord k => Zip (TotalSubsetMap s k)
deriving instance Ord k => ZipWithKey (TotalSubsetMap s k)
deriving instance Ord k => Lookup (TotalSubsetMap s k)
deriving instance Ord k => Indexable (TotalSubsetMap s k)
deriving instance Ord k => Adjustable (TotalSubsetMap s k)
deriving instance Ord k => FoldableWithKey (TotalSubsetMap s k)

-- | Complexity: 'traverseWithKey' O(n)
instance Ord k => TraversableWithKey (TotalSubsetMap s k) where
    traverseWithKey f (TotalSubsetMap m) = TotalSubsetMap <$> traverseWithKey f m

-- Linear instances.

-- | Complexity: all O(n)
instance (Ord k, Subset s k) => Additive (TotalSubsetMap s k) where
    zero = pure 0

-- | Complexity: all O(n)
instance (Ord k, Subset s k) => Metric (TotalSubsetMap s k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n * log n)
instance (Ord k, Subset s k) => Serial1 (TotalSubsetMap s k) where
    serializeWith f (TotalSubsetMap m) = serializeWith f (Map.elems m)
    deserializeWith f = do
        elems <- deserializeWith f
        let keys = reflect (Proxy :: Proxy s)
            assocs = zip (Set.toAscList keys) elems
        return $ TotalSubsetMap (Map.fromDistinctAscList assocs)

-- | Complexity: 'serialize' O(n), 'deserialize' O(n * log n)
instance (Ord k, Subset s k, Serial a) => Serial (TotalSubsetMap s k a) where
    serialize m = serializeWith serialize m
    deserialize = deserializeWith deserialize

-- Distributive and representable.

-- | Complexity: all O(n)
instance (Ord k, Subset s k) => Distributive (TotalSubsetMap s k) where
    distribute = TotalSubsetMap . Map.fromDistinctAscList
               . zip keys
               . distributeList . fmap asList
      where
        keys = Set.toAscList (reflect (Proxy :: Proxy s))
        asList (TotalSubsetMap m) = Map.elems m
        distributeList x = map (fmap head) $ iterate (fmap tail) x

-- | Convert from and to a partial function.
--
-- Complexity: tabulate O(n), index O(log n)
instance (Ord k, Subset s k) => Representable (TotalSubsetMap s k) where
    type Rep (TotalSubsetMap s k) = k
    tabulate f = TotalSubsetMap $ Map.fromSet f (reflect (Proxy :: Proxy s))
    index = Data.Key.index

-- | Restrict a partial map to a total map.
--
-- Complexity: O(n)
restrict :: forall k a r. Map k a
         -> (forall s. Subset s k => TotalSubsetMap s k a -> r)
         -> r
restrict m r = reify (Map.keysSet m) f
  where
    f :: forall s. Subset s k => Proxy s -> r
    f _ = r (TotalSubsetMap m :: TotalSubsetMap s k a)
