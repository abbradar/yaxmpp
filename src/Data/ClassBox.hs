{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ClassBox (
  Unconstrained,
  ClassBox (..),
) where

import Data.Kind

{- | A trivial constraint satisfied by all types.
Note: could be converted to @type Unconstrained a = (() :: Constraint)@
once UnsaturatedTypeFamilies is available.
-}
class Unconstrained a

instance Unconstrained a

data ClassBox (constr :: Type -> Constraint) = forall a. (constr a) => ClassBox {toClass :: a}

instance (forall a. (constr a) => Show a) => Show (ClassBox constr) where
  showsPrec d (ClassBox a) = showsPrec d a
  show (ClassBox a) = show a
  showList xs = showString "[" . go xs . showString "]"
   where
    go :: [ClassBox constr] -> ShowS
    go [] = id
    go [ClassBox x] = shows x
    go (ClassBox x : rest) = shows x . showString "," . go rest
