{-# LANGUAGE Strict #-}

module Data.ClassVector (
  ClassRef (..),
  ClassVector,
  empty,
  toAscList,
  toDescList,
  push,
  pushNewOrFailM,
  delete,
)
where

import Data.Kind
import Data.Typeable
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V

data ClassRef (constr :: Type -> Constraint) = forall a. (constr a) => ClassRef {toClass :: a}

newtype ClassVector (constr :: Type -> Constraint) = ClassVector (Vector (TypeRep, ClassRef constr))

empty :: ClassVector constr
empty = ClassVector V.empty

toAscList :: ClassVector constr -> [ClassRef constr]
toAscList (ClassVector vec) = map snd $ V.toList vec

vectorToDescList :: Vector a -> [a]
vectorToDescList vec = map (V.unsafeIndex vec) [len - 1, len - 2 .. 0]
 where
  len = V.length vec

toDescList :: ClassVector constr -> [ClassRef constr]
toDescList (ClassVector vec) = map snd $ vectorToDescList vec

findIndex :: TypeRep -> ClassVector constr -> Maybe Int
findIndex typ (ClassVector vec) = V.findIndex ((== typ) . fst) vec

push :: (Typeable a, constr a) => a -> ClassVector constr -> ClassVector constr
push val cls@(ClassVector vec) = ClassVector vec'
 where
  typ = typeOf val
  vec'
    | Just i <- findIndex typ cls = V.unsafeUpd vec [(i, (typ, ClassRef val))]
    | otherwise = V.snoc vec (typ, ClassRef val)

pushNewOrFailM :: (MonadFail m, Typeable a, constr a) => a -> ClassVector constr -> m (ClassVector constr)
pushNewOrFailM val cls@(ClassVector vec) = case findIndex typ cls of
  Just _ -> fail "pushNewOrFailM: type already exists"
  Nothing -> return $ ClassVector $ V.snoc vec (typ, ClassRef val)
 where
  typ = typeOf val

delete :: (Typeable a, constr a) => ClassVector constr -> a -> ClassVector constr
delete (ClassVector vec) val = ClassVector $ V.filter (\(t, _) -> t /= typeOf val) vec
