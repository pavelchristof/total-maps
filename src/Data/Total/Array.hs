{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Description :  Bounded, dense, total map implemented as a vector.
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Portability :  portable
--
-- Bounded, dense, total map implemented as a vector.
-----------------------------------------------------------------------------
module Data.Total.Array (
    TotalArray(..)
    ) where

import           Data.Bytes.Serial
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Key
import           Data.Proxy
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Linear
import           Prelude ()
import           Prelude.Compat hiding (zip, zipWith)

infixr 9 .:

-- | A total map from keys k to values a, represented as an immutable vector.
--
-- Warning: the number of keys MUST fit into an Int.
--
-- n is equal to the number of keys.
newtype TotalArray k a = TotalArray (Vector a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

keyCount :: forall k. (Enum k, Bounded k) => Proxy k -> Int
keyCount _ = fromEnum (maxBound :: k) - fromEnum (minBound :: k) + 1

toIndex :: forall k. (Enum k, Bounded k) => k -> Int
toIndex k = fromEnum k - fromEnum (minBound :: k)

fromIndex :: forall k. (Enum k, Bounded k) => Int -> k
fromIndex i = toEnum (i + fromEnum (minBound :: k))

keys :: forall k. (Enum k, Bounded k) => TotalArray k k
keys = TotalArray $ Vector.fromListN (keyCount (Proxy :: Proxy k)) [minBound .. maxBound]

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

-- | Zippy applicative. Complexity: 'pure' O(n), '<*>' O(n).
instance (Enum k, Bounded k) => Applicative (TotalArray k) where
    pure = TotalArray . Vector.replicate (keyCount (Proxy :: Proxy k))
    (<*>) = zap

-- Keys instances.

type instance Key (TotalArray k) = k

-- | Complexity: 'mapWithKey' O(n)
instance (Enum k, Bounded k) => Keyed (TotalArray k) where
    mapWithKey f v = zipWith f keys v

-- | Complexity: all O(n)
instance Zip (TotalArray k) where
    zipWith f (TotalArray a) (TotalArray b) =
        TotalArray $ Vector.zipWith f a b

-- | Complexity: all O(n)
instance (Enum k, Bounded k) => ZipWithKey (TotalArray k) where
    zipWithKey f a b = zipWith (uncurry . f) keys (zip a b)

-- | Complexity: 'lookup' O(1)
instance (Enum k, Bounded k) => Lookup (TotalArray k) where
    lookup k (TotalArray v) = Just $ Vector.unsafeIndex v (toIndex k)

-- | Complexity: 'index' O(1)
instance (Enum k, Bounded k) => Indexable (TotalArray k) where
    index (TotalArray v) k = Vector.unsafeIndex v (toIndex k)

-- | Complexity: 'adjust' O(n)
instance (Enum k, Bounded k) => Adjustable (TotalArray k) where
    adjust f k (TotalArray v) = TotalArray $ Vector.unsafeUpd v [(i, x)]
      where
        i = toIndex k
        x = f $ Vector.unsafeIndex v i

-- | Complexity: 'foldMapWithKey' O(n)
instance (Enum k, Bounded k) => FoldableWithKey (TotalArray k) where
    foldMapWithKey f v = foldMap (uncurry f) (zip keys v)

-- | Complexity: 'traverseWithKey' O(n)
instance (Enum k, Bounded k) => TraversableWithKey (TotalArray k) where
    traverseWithKey f v = traverse (uncurry f) (zip keys v)

-- Linear instances.

-- | Complexity: all O(n)
instance (Enum k, Bounded k) => Additive (TotalArray k) where
    zero = pure 0

-- | Complexity: all O(n)
instance (Enum k, Bounded k) => Metric (TotalArray k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n)
instance (Enum k, Bounded k) => Serial1 (TotalArray k) where
    serializeWith f (TotalArray v) = Vector.mapM_ f v
    deserializeWith f = TotalArray
        <$> Vector.replicateM (keyCount (Proxy :: Proxy k)) f

-- | Complexity: 'serialize' O(n), 'deserialize' O(n)
instance (Enum k, Bounded k, Serial a) => Serial (TotalArray k a) where
    serialize m = serializeWith serialize m
    deserialize = deserializeWith deserialize

-- | Complexity: 'distribute' O(n * fmap)
instance (Enum k, Bounded k) => Distributive (TotalArray k) where
    distribute x = TotalArray $ Vector.generate
        (keyCount (Proxy :: Proxy k)) (\i -> fmap (index' i) x)
      where
        index' i (TotalArray v) = Vector.unsafeIndex v i

-- | Convert from and to a total function.
--
-- Complexity: tabulate O(n), index O(1)
instance (Enum k, Bounded k) => Representable (TotalArray k) where
    type Rep (TotalArray k) = k
    tabulate f = fmap f keys
    index = Data.Key.index
