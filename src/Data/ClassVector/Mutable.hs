{-# LANGUAGE Strict #-}

-- | A mutable wrapper around 'ClassVector' using 'IORef'.
module Data.ClassVector.Mutable (
  ClassVectorRef,
  new,
  toAscList,
  toDescList,
  push,
  tryPushNew,
  pushNewOrFailM,
  delete,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ClassVector (ClassBox, ClassVector)
import qualified Data.ClassVector as CV
import Data.IORef
import Data.Typeable (Typeable)

newtype ClassVectorRef constr = ClassVectorRef (IORef (ClassVector constr))

new :: (MonadIO m) => m (ClassVectorRef constr)
new = ClassVectorRef <$> liftIO (newIORef CV.empty)

toAscList :: (MonadIO m) => ClassVectorRef constr -> m [ClassBox constr]
toAscList (ClassVectorRef ref) = CV.toAscList <$> liftIO (readIORef ref)

toDescList :: (MonadIO m) => ClassVectorRef constr -> m [ClassBox constr]
toDescList (ClassVectorRef ref) = CV.toDescList <$> liftIO (readIORef ref)

push :: (MonadIO m, Typeable a, constr a) => a -> ClassVectorRef constr -> m ()
push val (ClassVectorRef ref) = liftIO $ modifyIORef' ref (CV.push val)

-- | Try to push a new value. Returns 'True' if inserted, 'False' if the type already exists.
tryPushNew :: (MonadIO m, Typeable a, constr a) => a -> ClassVectorRef constr -> m Bool
tryPushNew val (ClassVectorRef ref) =
  liftIO $ atomicModifyIORef ref $ \vec ->
    case CV.tryPushNew val vec of
      Nothing -> (vec, False)
      Just vec' -> (vec', True)

pushNewOrFailM :: (MonadIO m, MonadFail m, Typeable a, constr a) => a -> ClassVectorRef constr -> m ()
pushNewOrFailM val cvr = do
  success <- tryPushNew val cvr
  unless success $ fail "pushNewOrFailM: type already exists"

delete :: (MonadIO m, Typeable a, constr a) => a -> ClassVectorRef constr -> m ()
delete val (ClassVectorRef ref) = liftIO $ modifyIORef' ref (`CV.delete` val)
