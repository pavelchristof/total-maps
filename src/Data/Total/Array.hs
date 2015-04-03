{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Description :  Bounded, dense, total map implemented as a vector.
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC >= 7.10
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

infixr 9 .:

-- | A total map from keys k to values a, represented as an immutable vector.
--
-- Warning: the number of keys MUST fit into an Int.
--
-- n is equal to the number of keys. 'fromEnum' and 'toEnum' are assumed to be O(k),
-- (which usually is O(1)).
newtype TotalArray k a = TotalArray (Vector a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

keyCount :: forall k. (Enum k, Bounded k) => Proxy k -> Int
keyCount _ = fromEnum (maxBound :: k) - fromEnum (minBound :: k) + 1

toIndex :: forall k. (Enum k, Bounded k) => k -> Int
toIndex k = fromEnum k - fromEnum (minBound :: k)

fromIndex :: forall k. (Enum k, Bounded k) => Int -> k
fromIndex i = toEnum (i + fromEnum (minBound :: k))

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

-- | Zippy applicative. Complexity: 'pure' O(n + k), '<*>' O(n).
instance (Enum k, Bounded k) => Applicative (TotalArray k) where
    pure = TotalArray . Vector.replicate (keyCount (Proxy :: Proxy k))
    (<*>) = zap

-- Keys instances.

type instance Key (TotalArray k) = k

-- | Complexity: 'mapWithKey' O(n * k)
instance (Enum k, Bounded k) => Keyed (TotalArray k) where
    mapWithKey f (TotalArray v) = TotalArray $ Vector.imap (f . fromIndex) v

-- | Complexity: all O(n)
instance Zip (TotalArray k) where
    zipWith f (TotalArray a) (TotalArray b) =
        TotalArray $ Vector.zipWith f a b

-- | Complexity: all O(n * k)
instance (Enum k, Bounded k) => ZipWithKey (TotalArray k) where
    zipWithKey f (TotalArray a) (TotalArray b) =
        TotalArray $ Vector.izipWith (f . fromIndex) a b

-- | Complexity: 'lookup' O(k)
instance (Enum k, Bounded k) => Lookup (TotalArray k) where
    lookup k (TotalArray v) = Just $ Vector.unsafeIndex v (toIndex k)

-- | Complexity: 'index' O(k)
instance (Enum k, Bounded k) => Indexable (TotalArray k) where
    index (TotalArray v) k = Vector.unsafeIndex v (toIndex k)

-- | Complexity: 'adjust' O(n + k)
instance (Enum k, Bounded k) => Adjustable (TotalArray k) where
    adjust f k (TotalArray v) = TotalArray $ Vector.unsafeUpd v [(i, x)]
      where
        i = toIndex k
        x = f $ Vector.unsafeIndex v i

-- | Complexity: 'foldMapWithKey' O(n * k)
instance (Enum k, Bounded k) => FoldableWithKey (TotalArray k) where
    foldMapWithKey f (TotalArray v) =
        Vector.ifoldr (mappend .: f . fromIndex) mempty v

-- | Complexity: 'traverseWithKey' O(n * k)
instance (Enum k, Bounded k) => TraversableWithKey (TotalArray k) where
    traverseWithKey f (TotalArray v) =
        TotalArray <$> traverse (\(i, x) -> f (fromIndex i) x) (Vector.indexed v)

-- Linear instances.

-- | Complexity: 'zero' O(n + k), rest O(n)
instance (Enum k, Bounded k) => Additive (TotalArray k) where
    zero = pure 0

-- | Complexity: all O(n)
instance (Enum k, Bounded k) => Metric (TotalArray k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n + k)
instance (Enum k, Bounded k) => Serial1 (TotalArray k) where
    serializeWith f (TotalArray v) = Vector.mapM_ f v
    deserializeWith f = TotalArray
        <$> Vector.replicateM (keyCount (Proxy :: Proxy k)) f

-- | Complexity: 'serialize' O(n), 'deserialize' O(n + k)
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
-- Complexity: tabulate O(n), index O(log n)
instance (Enum k, Bounded k) => Representable (TotalArray k) where
    type Rep (TotalArray k) = k
    tabulate f = TotalArray $ Vector.generate
        (keyCount (Proxy :: Proxy k)) (f . fromIndex)
    index = Data.Key.index
