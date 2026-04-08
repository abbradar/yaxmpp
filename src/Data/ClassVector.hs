{-# LANGUAGE Strict #-}

module Data.ClassVector (
  ClassBox (..),
  ClassVector,
  empty,
  toAscList,
  toDescList,
  push,
  tryPushNew,
  pushNewOrFailM,
  delete,
)
where

import Data.Kind
import Data.Typeable
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V

data ClassBox (constr :: Type -> Constraint) = forall a. (constr a) => ClassBox {toClass :: a}

newtype ClassVector (constr :: Type -> Constraint) = ClassVector (Vector (TypeRep, ClassBox constr))

empty :: ClassVector constr
empty = ClassVector V.empty

toAscList :: ClassVector constr -> [ClassBox constr]
toAscList (ClassVector vec) = map snd $ V.toList vec

vectorToDescList :: Vector a -> [a]
vectorToDescList vec = map (V.unsafeIndex vec) [len - 1, len - 2 .. 0]
 where
  len = V.length vec

toDescList :: ClassVector constr -> [ClassBox constr]
toDescList (ClassVector vec) = map snd $ vectorToDescList vec

findIndex :: TypeRep -> ClassVector constr -> Maybe Int
findIndex typ (ClassVector vec) = V.findIndex ((== typ) . fst) vec

push :: (Typeable a, constr a) => a -> ClassVector constr -> ClassVector constr
push val cls@(ClassVector vec) = ClassVector vec'
 where
  typ = typeOf val
  vec'
    | Just i <- findIndex typ cls = V.unsafeUpd vec [(i, (typ, ClassBox val))]
    | otherwise = V.snoc vec (typ, ClassBox val)

{- | Try to push a new value. Returns 'Just' the updated vector if inserted,
or 'Nothing' if the type already exists.
-}
tryPushNew :: (Typeable a, constr a) => a -> ClassVector constr -> Maybe (ClassVector constr)
tryPushNew val cls@(ClassVector vec) = case findIndex typ cls of
  Just _ -> Nothing
  Nothing -> Just $ ClassVector $ V.snoc vec (typ, ClassBox val)
 where
  typ = typeOf val

pushNewOrFailM :: (MonadFail m, Typeable a, constr a) => a -> ClassVector constr -> m (ClassVector constr)
pushNewOrFailM val vec = case tryPushNew val vec of
  Nothing -> fail "pushNewOrFailM: type already exists"
  Just vec' -> return vec'

delete :: (Typeable a, constr a) => ClassVector constr -> a -> ClassVector constr
delete (ClassVector vec) val = ClassVector $ V.filter (\(t, _) -> t /= typeOf val) vec
