{-# LANGUAGE Strict #-}

module Data.GenericRegistry (
  GenericRegistry,
  empty,
  insert,
  insertNewOrFailM,
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

newtype GenericRegistry (f :: Type -> Type) = GenericRegistry (Map TypeRep Any)

empty :: GenericRegistry f
empty = GenericRegistry M.empty

insert :: (Typeable a) => Proxy a -> f a -> GenericRegistry f -> GenericRegistry f
insert k v (GenericRegistry m) = GenericRegistry (M.insert (typeRep k) (unsafeCoerce v) m)

insertNewOrFailM :: (Typeable a, MonadFail m) => Proxy a -> f a -> GenericRegistry f -> m (GenericRegistry f)
insertNewOrFailM k v (GenericRegistry m)
  | Just _ <- M.lookup typ m = fail "Key already exists"
  | otherwise = return $ GenericRegistry (M.insert typ (unsafeCoerce v) m)
 where
  typ = typeRep k

delete :: (Typeable a) => Proxy a -> GenericRegistry f -> GenericRegistry f
delete k (GenericRegistry m) = GenericRegistry (M.delete (typeRep k) m)

lookup :: (Typeable a) => Proxy a -> GenericRegistry f -> Maybe (f a)
lookup k (GenericRegistry m) = fmap unsafeCoerce $ M.lookup (typeRep k) m
