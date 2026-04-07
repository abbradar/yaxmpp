{-# LANGUAGE Strict #-}

module Data.DynamicMap (
  DynamicMap,
  empty,
  insert,
  delete,
  lookup,
)
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy)
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Exts (Any)
import Unsafe.Coerce
import Prelude hiding (lookup)

newtype DynamicMap (f :: Type -> Type) = DynamicMap (Map TypeRep Any)

empty :: DynamicMap f
empty = DynamicMap M.empty

insert :: (Typeable a) => Proxy a -> f a -> DynamicMap f -> DynamicMap f
insert k v (DynamicMap m) = DynamicMap (M.insert (typeRep k) (unsafeCoerce v) m)

delete :: (Typeable a) => Proxy a -> DynamicMap f -> DynamicMap f
delete k (DynamicMap m) = DynamicMap (M.delete (typeRep k) m)

lookup :: (Typeable a) => Proxy a -> DynamicMap f -> Maybe (f a)
lookup k (DynamicMap m) = fmap unsafeCoerce $ M.lookup (typeRep k) m
