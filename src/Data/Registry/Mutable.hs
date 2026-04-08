{-# LANGUAGE Strict #-}

-- | A mutable wrapper around 'Registry' using 'IORef'.
module Data.Registry.Mutable (
  RegistryRef,
  new,
  read,
  insert,
  tryInsertNew,
  insertNewOrFailM,
  delete,
  lookup,
  lookupOrFailM,
  pop,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Kind (Constraint, Type)
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import Data.Typeable (Typeable)
import Prelude hiding (lookup, null, read)

newtype RegistryRef (constr :: Type -> Constraint) = RegistryRef (IORef (Registry constr))

new :: (MonadIO m) => m (RegistryRef constr)
new = RegistryRef <$> liftIO (newIORef Reg.empty)

read :: (MonadIO m) => RegistryRef constr -> m (Registry constr)
read (RegistryRef ref) = liftIO $ readIORef ref

insert :: (MonadIO m, Typeable a, constr a) => a -> RegistryRef constr -> m ()
insert v (RegistryRef ref) = liftIO $ modifyIORef' ref (Reg.insert v)

tryInsertNew :: (MonadIO m, Typeable a, constr a) => a -> RegistryRef constr -> m Bool
tryInsertNew v (RegistryRef ref) =
  liftIO $ atomicModifyIORef ref $ \reg ->
    case Reg.tryInsertNew v reg of
      Nothing -> (reg, False)
      Just reg' -> (reg', True)

insertNewOrFailM :: (MonadIO m, MonadFail m, Typeable a, constr a) => a -> RegistryRef constr -> m ()
insertNewOrFailM v rr = do
  success <- tryInsertNew v rr
  unless success $ fail "insertNewOrFailM: type already exists"

delete :: (MonadIO m, Typeable a) => Proxy a -> RegistryRef constr -> m ()
delete k (RegistryRef ref) = liftIO $ modifyIORef' ref (Reg.delete k)

lookup :: (MonadIO m, Typeable a) => Proxy a -> RegistryRef constr -> m (Maybe a)
lookup k (RegistryRef ref) = Reg.lookup k <$> liftIO (readIORef ref)

lookupOrFailM :: (MonadIO m, MonadFail m, Typeable a) => Proxy a -> RegistryRef constr -> m a
lookupOrFailM k rr = do
  mv <- lookup k rr
  case mv of
    Just v -> return v
    Nothing -> fail "lookupOrFailM: key does not exist"

pop :: (MonadIO m, Typeable a) => Proxy a -> RegistryRef constr -> m (Maybe a)
pop k (RegistryRef ref) =
  liftIO $ atomicModifyIORef ref $ \reg ->
    let (mv, reg') = Reg.pop k reg
     in (reg', mv)
