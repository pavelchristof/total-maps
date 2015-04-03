{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Description :  Subset, dense, total map implemented as a vector.
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC >= 7.10
--
-- Subset, dense, total map implemented as a vector.
-----------------------------------------------------------------------------
module Data.Total.Array.Subset (
    Subset,
    TotalSubsetArray(..),
    keys
    ) where

import           Data.Bytes.Serial
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Key
import           Data.Proxy
import           Data.Reflection
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Total.Subset
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Linear
import           Prelude hiding (zip, zipWith)

infixr 9 .:

-- | A total map from a subset s of keys k to values a, e.g. a restriction
-- of a partial function @k -> a@ to a subset of its domain on which the
-- function is defined. Implemented as a vector.
--
-- n is equal to the number of keys.
newtype TotalSubsetArray s k a = TotalSubsetArray (Vector a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

keyCount :: Subset s k => Proxy s -> Int
keyCount p = Set.size (reflect p)

keys' :: Subset s k => Proxy s -> Vector k
keys' p = Vector.fromListN (keyCount p) $ Set.toAscList (reflect p)

toIndex :: (Ord k, Subset s k) => Proxy s -> k -> Int
toIndex p k = Set.findIndex k (reflect p)

-- | Maps each key to itself.
keys :: forall s k. Subset s k => TotalSubsetArray s k k
keys = TotalSubsetArray (keys' (Proxy :: Proxy s))

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

-- | Zippy applicative. Complexity: 'pure' O(n), '<*>' O(n).
instance Subset s k => Applicative (TotalSubsetArray s k) where
    pure = TotalSubsetArray . Vector.replicate (keyCount (Proxy :: Proxy s))
    (<*>) = zap

-- Keys instances.

type instance Key (TotalSubsetArray s k) = k

-- | Complexity: 'mapWithKey' O(n)
instance Subset s k => Keyed (TotalSubsetArray s k) where
    mapWithKey f v = zipWith f keys v

-- | Complexity: all O(n)
instance Zip (TotalSubsetArray s k) where
    zipWith f (TotalSubsetArray a) (TotalSubsetArray b) =
        TotalSubsetArray $ Vector.zipWith f a b

-- | Complexity: all O(n)
instance Subset s k => ZipWithKey (TotalSubsetArray s k) where
    zipWithKey f a b = zipWith (uncurry . f) keys (zip a b)

-- | Complexity: 'lookup' O(log n)
instance (Ord k, Subset s k) => Lookup (TotalSubsetArray s k) where
    lookup k (TotalSubsetArray v) =
        Just $ Vector.unsafeIndex v (toIndex (Proxy :: Proxy s) k)

-- | Complexity: 'index' O(log n)
instance (Ord k, Subset s k) => Indexable (TotalSubsetArray s k) where
    index (TotalSubsetArray v) k =
        Vector.unsafeIndex v (toIndex (Proxy :: Proxy s) k)

-- | Complexity: 'adjust' O(n)
instance (Ord k, Subset s k) => Adjustable (TotalSubsetArray s k) where
    adjust f k (TotalSubsetArray v) = TotalSubsetArray $ Vector.unsafeUpd v [(i, x)]
      where
        i = toIndex (Proxy :: Proxy s) k
        x = f $ Vector.unsafeIndex v i

-- | Complexity: 'foldMapWithKey' O(n)
instance Subset s k => FoldableWithKey (TotalSubsetArray s k) where
    foldMapWithKey f v = foldMap (uncurry f) (zip keys v)

-- | Complexity: 'traverseWithKey' O(n)
instance Subset s k => TraversableWithKey (TotalSubsetArray s k) where
    traverseWithKey f v = traverse (uncurry f) (zip keys v)

-- Linear instances.

-- | Complexity: all O(n)
instance Subset s k => Additive (TotalSubsetArray s k) where
    zero = pure 0

-- | Complexity: all O(n)
instance Subset s k => Metric (TotalSubsetArray s k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n)
instance Subset s k => Serial1 (TotalSubsetArray s k) where
    serializeWith f (TotalSubsetArray v) = Vector.mapM_ f v
    deserializeWith f = TotalSubsetArray
        <$> Vector.replicateM (keyCount (Proxy :: Proxy s)) f

-- | Complexity: 'serialize' O(n), 'deserialize' O(n)
instance (Subset s k, Serial a) => Serial (TotalSubsetArray s k a) where
    serialize m = serializeWith serialize m
    deserialize = deserializeWith deserialize

-- | Complexity: 'distribute' O(n * fmap)
instance Subset s k => Distributive (TotalSubsetArray s k) where
    distribute x = TotalSubsetArray $ Vector.generate
        (keyCount (Proxy :: Proxy s)) (\i -> fmap (index' i) x)
      where
        index' i (TotalSubsetArray v) = Vector.unsafeIndex v i

-- | Convert from and to a total function.
--
-- Complexity: tabulate O(n), index O(log n)
instance (Ord k, Subset s k) => Representable (TotalSubsetArray s k) where
    type Rep (TotalSubsetArray s k) = k
    tabulate f = fmap f keys
    index = Data.Key.index
