{-# LANGUAGE Strict #-}

module Data.Registry (
  Registry,
  empty,
  insert,
  insertNewOrFailM,
  delete,
  lookup,
)
where

import Data.Functor.Identity
import Data.GenericRegistry (GenericRegistry)
import qualified Data.GenericRegistry as GR
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Prelude hiding (lookup)

newtype Registry = Registry (GenericRegistry Identity)

empty :: Registry
empty = Registry GR.empty

insert :: (Typeable a) => a -> Registry -> Registry
insert v (Registry m) = Registry $ GR.insert (Proxy :: Proxy a) (Identity v) m

insertNewOrFailM :: (Typeable a, MonadFail m) => Proxy a -> a -> Registry -> m Registry
insertNewOrFailM k v (Registry m) = fmap Registry $ GR.insertNewOrFailM k (Identity v) m

delete :: (Typeable a) => Proxy a -> Registry -> Registry
delete k (Registry m) = Registry $ GR.delete k m

lookup :: (Typeable a) => Proxy a -> Registry -> Maybe a
lookup k (Registry m) = fmap runIdentity $ GR.lookup k m
