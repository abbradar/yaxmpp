{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | A heterogeneous registry keyed by type, with a constraint on values.

Note: this could be reimplemented on top of GenericRegistry using
UnsaturatedTypeFamilies once that extension is available.
-}
module Data.Registry (
  Registry,
  RegistryConstraint,
  empty,
  null,
  insert,
  tryInsertNew,
  insertNewOrFailM,
  delete,
  lookup,
  toList,
) where

import Data.ClassBox (ClassBox (..))
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup, null)

-- | The effective constraint on values in a Registry: both Typeable and the user-supplied constraint.
class (Typeable a, constr a) => RegistryConstraint constr a

instance (Typeable a, constr a) => RegistryConstraint constr a

newtype Registry (constr :: Type -> Constraint) = Registry (Map TypeRep (ClassBox (RegistryConstraint constr)))

empty :: Registry constr
empty = Registry M.empty

null :: Registry constr -> Bool
null (Registry m) = M.null m

insert :: (Typeable a, constr a) => a -> Registry constr -> Registry constr
insert v (Registry m) = Registry (M.insert (typeOf v) (ClassBox v) m)

tryInsertNew :: forall a constr. (Typeable a, constr a) => a -> Registry constr -> Maybe (Registry constr)
tryInsertNew v (Registry m)
  | Just _ <- M.lookup typ m = Nothing
  | otherwise = Just $ Registry (M.insert typ (ClassBox v) m)
 where
  typ = typeOf v

insertNewOrFailM :: (Typeable a, constr a, MonadFail m) => a -> Registry constr -> m (Registry constr)
insertNewOrFailM v reg = case tryInsertNew v reg of
  Nothing -> fail "insertNewOrFailM: key already exists"
  Just reg' -> return reg'

delete :: (Typeable a) => Proxy a -> Registry constr -> Registry constr
delete k (Registry m) = Registry (M.delete (typeRep k) m)

lookup :: (Typeable a) => Proxy a -> Registry constr -> Maybe a
lookup k (Registry m) = case M.lookup (typeRep k) m of
  Just (ClassBox v) -> Just (unsafeCoerce v)
  Nothing -> Nothing

toList :: Registry constr -> [ClassBox (RegistryConstraint constr)]
toList (Registry m) = M.elems m
