{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Description :  Bounded sparse total map.
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC >= 7.10
--
-- Sparse total maps for bounded types.
-----------------------------------------------------------------------------
module Data.Total.Map.Sparse where

import           Data.Bytes.Serial
import           Data.Key
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid (First(..))
import           Data.Semigroup hiding (First, getFirst)
import           Data.Total.Internal.SparseFold
import           Linear
import           Prelude hiding (zip, lookup)

-- | A total sparse map from keys k to values a. This map is implemented as a
-- partial map and a default value. 'pure' creates an all-default values map
-- with the given default value.
--
-- n is equal to the number of keys, k is the number of non-default values.
-- If there are two maps involved k is taken to be the number of non-default
-- values of their union.
data TotalSparseMap k a = TotalSparseMap (Map k a) a
    deriving (Show, Read, Functor)

-- | Complexity: O(k * log (n/k)) - arises from fold
instance (Ord k, Enum k, Bounded k, Eq a) => Eq (TotalSparseMap k a) where
    a == b = and ((==) <$> a <*> b)

-- | Complexity: O(k * log (n/k)) - arises from fold
instance (Ord k, Enum k, Bounded k, Ord a) => Ord (TotalSparseMap k a) where
    compare a b = fromMaybe EQ $ getFirst $ foldMap (First . notEq) (compare <$> a <*> b)
      where
        notEq EQ = Nothing
        notEq x  = Just x

-- | Zippy applicative. Complexity: 'pure' O(1), '<*>' O(k1 + k2)
instance Ord k => Applicative (TotalSparseMap k) where
    pure x = TotalSparseMap Map.empty x
    (<*>)  = zap

-- | Folds over the whole domain, including the default values.
--
-- >>> sum (pure 1 :: TotalSparseMap Int Integer)
-- 18446744073709551616
--
-- Complexity: foldMap O(k * log (n/k)), the rest are defined using foldMap
instance (Ord k, Enum k, Bounded k) => Foldable (TotalSparseMap k) where
    foldMap f (TotalSparseMap m d) = runSparseFold (f d) $ \_ ->
           foldPoint (toInteger (fromEnum (minBound :: k)) - 1) mempty
        <> Map.foldMapWithKey (\k v -> foldPoint (toInteger (fromEnum k)) (f v)) m
        <> foldPoint (toInteger (fromEnum (maxBound :: k)) + 1) mempty

-- Keys instances.

type instance Key (TotalSparseMap k) = k

-- | Complexity: 'lookup' O(log k)
instance Ord k => Lookup (TotalSparseMap k) where
    lookup k (TotalSparseMap m d) =
      case lookup k m of
        Nothing -> Just d
        x -> x

-- | Complexity: 'index' O(log k)
instance Ord k => Indexable (TotalSparseMap k) where
    index (TotalSparseMap m d) k =
      case lookup k m of
        Nothing -> d
        Just x -> x

-- | Complexity: all O(log k)
instance Ord k => Adjustable (TotalSparseMap k) where
    adjust f k (TotalSparseMap m d) = TotalSparseMap (Map.alter f' k m) d
      where
        f' (Just x) = Just (f x)
        f' Nothing = Just (f d)
    replace k v (TotalSparseMap m d) = TotalSparseMap (replace k v m) d

-- | Complexity: all O(k1 + k2)
instance Ord k => Zip (TotalSparseMap k) where
    zip (TotalSparseMap m1 d1) (TotalSparseMap m2 d2) =
      TotalSparseMap
        (Map.mergeWithKey
          (\_ a b -> Just (a, b))
          (fmap (, d2))
          (fmap (d1, ))
          m1 m2)
        (d1, d2)

-- Linear instances.

-- | Complexity: 'zero' O(1), rest O(k1 + k2)
instance Ord k => Additive (TotalSparseMap k) where
    zero = pure 0

-- | Complexity: all O(k * log (n/k)) - arises from fold
instance (Ord k, Enum k, Bounded k) => Metric (TotalSparseMap k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n * log n)
instance (Ord k, Enum k, Bounded k, Serial k) => Serial1 (TotalSparseMap k) where
    serializeWith f (TotalSparseMap m d) = do
        serializeWith f m
        f d
    deserializeWith f = TotalSparseMap
        <$> deserializeWith f
        <*> f

-- | Complexity: 'serialize' O(n), 'deserialize' O(n * log n)
instance (Ord k, Enum k, Bounded k, Serial k, Serial a)
         => Serial (TotalSparseMap k a) where
    serialize m = serializeWith serialize m
    deserialize = deserializeWith deserialize
