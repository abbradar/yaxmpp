{-# LANGUAGE Strict #-}

module Data.RefMap (
  RefMap,
  new,
  entries,
  reverseEntries,
  EntryId,
  add,
  delete,
)
where

import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.IORef

data RefMapInternal a = RefMapInternal
  { entriesMap :: Map Int a
  , lastId :: Int
  }

newtype RefMap a = RefMap (IORef (RefMapInternal a))

newtype EntryId = EntryId Int

new :: (MonadIO m) => m (RefMap a)
new = RefMap <$> newIORef empty
 where
  empty =
    RefMapInternal
      { entriesMap = M.empty
      , lastId = 0
      }

entries :: (MonadIO m) => RefMap a -> m [a]
entries (RefMap r) = do
  f <- readIORef r
  return $ M.elems $ entriesMap f

reverseEntries :: (MonadIO m) => RefMap a -> m [a]
reverseEntries (RefMap r) = do
  f <- readIORef r
  return $ map snd $ M.toDescList $ entriesMap f

add :: (MonadIO m) => RefMap a -> a -> m EntryId
add (RefMap r) handler =
  atomicModifyIORef' r $ \int ->
    let newInt =
          RefMapInternal
            { entriesMap = M.insert (lastId int) handler $ entriesMap int
            , lastId = lastId int + 1
            }
     in (newInt, EntryId $ lastId int)

delete :: (MonadIO m) => RefMap a -> EntryId -> m ()
delete (RefMap r) (EntryId hid) =
  atomicModifyIORef' r $ \int -> (int {entriesMap = M.delete hid $ entriesMap int}, ())
