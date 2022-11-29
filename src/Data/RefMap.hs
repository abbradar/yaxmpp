{-# LANGUAGE Strict #-}

module Data.RefMap
  ( RefMap
  , new
  , entries
  , RefMapRef
  , EntryId
  , ref
  , add
  , delete
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.IORef
import Control.Monad.IO.Class

data RefMapInternal a = RefMapInternal { entriesMap :: Map Int a
                                       , lastId :: Int
                                       }

newtype RefMap a = RefMap (IORef (RefMapInternal a))

newtype RefMapRef a = RefMapRef (IORef (RefMapInternal a))

newtype EntryId = EntryId Int

new :: MonadIO m => m (RefMap a)
new = RefMap <$> newIORef empty
  where empty = RefMapInternal { entriesMap = M.empty
                               , lastId = 0
                               }

entries :: MonadIO m => RefMap a -> m [a]
entries (RefMap r) = do
  f <- readIORef r
  return $ M.elems $ entriesMap f

ref :: RefMap a -> RefMapRef a
ref (RefMap r) = RefMapRef r

add :: MonadIO m => RefMapRef a -> a -> m EntryId
add (RefMapRef r) handler =
  atomicModifyIORef' r $ \int ->
    let newInt = RefMapInternal { entriesMap = M.insert (lastId int) handler $ entriesMap int
                                , lastId = lastId int + 1
                                }
    in (newInt, EntryId $ lastId int)

delete :: MonadIO m => RefMapRef a -> EntryId -> m ()
delete (RefMapRef r) (EntryId hid) =
  atomicModifyIORef' r $ \int -> (int { entriesMap = M.delete hid $ entriesMap int }, ())
