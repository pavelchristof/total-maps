{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Dense total maps for bounded types.
-----------------------------------------------------------------------------
module Data.Total.Map where

import           Control.Applicative
import           Data.Bytes.Serial
import           Data.Key
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Linear
import           Prelude hiding (zip)

-- | A total map from keys k to values a.
--
-- Most functions are derived from 'Data.Map.Map'.
--
-- Unfortunately I cannot find any law linking Enum with Ord, so we cannot
-- be sure that succ x > x. Because of that functions 'pure', 'deserialize',
-- 'deserializeWith' have complexity O(n * log n), while they could be O(n).
newtype TotalMap k a = TotalMap (Map k a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type instance Key (TotalMap k) = k

-- | Zippy applicative. Complexity: 'pure' O(n * log n), '<*>' O(n).
instance (Ord k, Enum k, Bounded k) => Applicative (TotalMap k) where
    pure x = TotalMap $ Map.fromList [(k, x) | k <- [minBound .. maxBound]]
    (<*>)  = zap

-- Keys instances.

-- TODO: it would be nice to document these, but haddock doesn't allow that.

deriving instance Keyed (TotalMap k)
deriving instance Ord k => Zip (TotalMap k)
deriving instance Ord k => ZipWithKey (TotalMap k)
deriving instance Ord k => Lookup (TotalMap k)
deriving instance Ord k => Indexable (TotalMap k)
deriving instance Ord k => Adjustable (TotalMap k)
deriving instance Ord k => FoldableWithKey (TotalMap k)

-- | Complexity: 'traverseWithKey' O(n)
instance Ord k => TraversableWithKey (TotalMap k) where
    traverseWithKey f (TotalMap m) = TotalMap <$> traverseWithKey f m

-- Linear instances.

-- | Complexity: 'zero' O(n * log n), rest O(n)
instance (Ord k, Enum k, Bounded k) => Additive (TotalMap k) where
    zero = pure 0

-- | Complexity: all O(n)
instance (Ord k, Enum k, Bounded k) => Metric (TotalMap k)

-- Serial instances.

-- | Complexity: 'serializeWith' O(n), 'deserializeWith' O(n * log n)
instance (Ord k, Enum k, Bounded k) => Serial1 (TotalMap k) where
    serializeWith f (TotalMap m) = serializeWith f (Map.elems m)
    deserializeWith f = do
        elems <- deserializeWith f
        let assocs = zip (sort [minBound .. maxBound]) elems
        return $ TotalMap (Map.fromDistinctAscList assocs)

-- | Complexity: 'serialize' O(n), 'deserialize' O(n * log n)
instance (Ord k, Enum k, Bounded k, Serial a) => Serial (TotalMap k a) where
    serialize m = serializeWith serialize m
    deserialize = deserializeWith deserialize
