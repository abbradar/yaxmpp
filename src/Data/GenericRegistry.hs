{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GenericRegistry (
  GenericRegistry,
  empty,
  null,
  insert,
  tryInsertNew,
  insertNewOrFailM,
  delete,
  lookup,
  pop,
  toList,
) where

import Data.ClassBox (ClassBox (..), showClassBoxList)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup, null)

newtype GenericRegistry (f :: Type -> Type) (constr :: Type -> Constraint) = GenericRegistry (Map TypeRep (ClassBox constr))

instance (forall a. (constr a) => Show a) => Show (GenericRegistry f constr) where
  showsPrec d (GenericRegistry m) =
    showParen (d > 10) $
      showString "GenericRegistry " . showClassBoxList (M.elems m)
  show (GenericRegistry m) = "GenericRegistry " ++ showClassBoxList (M.elems m) ""
  showList xs = showString "[" . go xs . showString "]"
   where
    go :: [GenericRegistry f constr] -> ShowS
    go [] = id
    go [GenericRegistry m] = showString "GenericRegistry " . showClassBoxList (M.elems m)
    go (GenericRegistry m : rest) = showString "GenericRegistry " . showClassBoxList (M.elems m) . showString "," . go rest

empty :: GenericRegistry f constr
empty = GenericRegistry M.empty

null :: GenericRegistry f constr -> Bool
null (GenericRegistry m) = M.null m

insert :: (Typeable a, constr (f a)) => Proxy a -> f a -> GenericRegistry f constr -> GenericRegistry f constr
insert k v (GenericRegistry m) = GenericRegistry (M.insert (typeRep k) (ClassBox v) m)

tryInsertNew :: (Typeable a, constr (f a)) => Proxy a -> f a -> GenericRegistry f constr -> Maybe (GenericRegistry f constr)
tryInsertNew k v (GenericRegistry m)
  | Just _ <- M.lookup typ m = Nothing
  | otherwise = Just $ GenericRegistry (M.insert typ (ClassBox v) m)
 where
  typ = typeRep k

insertNewOrFailM :: (Typeable a, constr (f a), MonadFail m) => Proxy a -> f a -> GenericRegistry f constr -> m (GenericRegistry f constr)
insertNewOrFailM k v reg = case tryInsertNew k v reg of
  Nothing -> fail "insertNewOrFailM: key already exists"
  Just reg' -> return reg'

delete :: (Typeable a) => Proxy a -> GenericRegistry f constr -> GenericRegistry f constr
delete k (GenericRegistry m) = GenericRegistry (M.delete (typeRep k) m)

lookup :: (Typeable a) => Proxy a -> GenericRegistry f constr -> Maybe (f a)
lookup k (GenericRegistry m) = case M.lookup (typeRep k) m of
  Just (ClassBox v) -> Just (unsafeCoerce v)
  Nothing -> Nothing

-- | Look up and remove a value from the registry.
pop :: (Typeable a) => Proxy a -> GenericRegistry f constr -> (Maybe (f a), GenericRegistry f constr)
pop k (GenericRegistry m) = case M.lookup (typeRep k) m of
  Just (ClassBox v) -> (Just (unsafeCoerce v), GenericRegistry (M.delete (typeRep k) m))
  Nothing -> (Nothing, GenericRegistry m)

toList :: GenericRegistry f constr -> [ClassBox constr]
toList (GenericRegistry m) = M.elems m
