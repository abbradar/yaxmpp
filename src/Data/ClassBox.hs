{-# LANGUAGE Strict #-}

module Data.ClassBox (
  ClassBox (..),
  showClassBox,
) where

import Data.Kind

data ClassBox (constr :: Type -> Constraint) = forall a. (constr a) => ClassBox {toClass :: a}

-- | Show a ClassBox given a way to show the contents.
showClassBox :: (forall a. (constr a) => a -> String) -> ClassBox constr -> String
showClassBox f (ClassBox a) = f a
