{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- License     :  MIT
-- Maintainer  :  Paweł Nowak <pawel834@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC only
--
-- Subset relation.
-----------------------------------------------------------------------------
module Data.Total.Subset where

import Data.Reflection
import Data.Set (Set)

-- | @Subset s k@ means that @s@ reifies a subset of @k@.
type Subset s k = Reifies s (Set k)
