{-# LANGUAGE Strict #-}

module Data.DynamicSet (
  DynamicSet,
  empty,
  insert,
  delete,
  lookup,
)
where

import Data.DynamicMap (DynamicMap)
import qualified Data.DynamicMap as DM
import Data.Functor.Identity
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Prelude hiding (lookup)

newtype DynamicSet = DynamicSet (DynamicMap Identity)

empty :: DynamicSet
empty = DynamicSet DM.empty

insert :: (Typeable a) => a -> DynamicSet -> DynamicSet
insert v (DynamicSet m) = DynamicSet $ DM.insert (Proxy :: Proxy a) (Identity v) m

delete :: (Typeable a) => Proxy a -> DynamicSet -> DynamicSet
delete k (DynamicSet m) = DynamicSet $ DM.delete k m

lookup :: (Typeable a) => Proxy a -> DynamicSet -> Maybe a
lookup k (DynamicSet m) = fmap runIdentity $ DM.lookup k $ m
