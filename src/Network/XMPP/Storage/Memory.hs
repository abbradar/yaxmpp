{-# LANGUAGE Strict #-}

-- | Reference 'MessageStorage' backed by an in-process 'IORef'.
module Network.XMPP.Storage.Memory (
  MemoryStorage,
  MemoryRoom,
  newMemoryStorage,
) where

import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime)
import UnliftIO.IORef

import Network.XMPP.Address
import Network.XMPP.Stanza
import Network.XMPP.Storage.Class

{- | Position of an entry in a room's total order: (timestamp, insertion seq).
Using the insertion sequence as a tiebreaker gives a stable order even when
multiple stanzas share a timestamp.
-}
type Position = (UTCTime, Word)

data MemRoomState = MemRoomState
  { mrsByKey :: Map (InStanzaType, Text) Position
  -- ^ Dedup + id→position resolver.
  , mrsByOrder :: Map Position StoredInStanza
  -- ^ Primary timestamp-ordered store.
  , mrsNextSeq :: Word
  }

newtype MemoryRoom = MemoryRoom (IORef MemRoomState)

newtype MemoryStorage = MemoryStorage (IORef (Map XMPPAddress MemoryRoom))

newMemoryStorage :: (MonadIO m) => m MemoryStorage
newMemoryStorage = liftIO $ MemoryStorage <$> newIORef M.empty

emptyRoomState :: MemRoomState
emptyRoomState =
  MemRoomState
    { mrsByKey = M.empty
    , mrsByOrder = M.empty
    , mrsNextSeq = 0
    }

instance (MonadIO m) => MessageStorage MemoryStorage m where
  type MessageStorageRoom MemoryStorage = MemoryRoom
  getOrCreateRoom (MemoryStorage ref) addr = liftIO $ do
    existing <- M.lookup addr <$> readIORef ref
    case existing of
      Just r -> return r
      Nothing -> do
        roomRef <- newIORef emptyRoomState
        let fresh = MemoryRoom roomRef
        -- Atomic insert-or-keep: if another thread raced us, keep theirs.
        atomicModifyIORef' ref $ \m -> case M.lookup addr m of
          Just r -> (m, r)
          Nothing -> (M.insert addr fresh m, fresh)

instance (MonadIO m) => RoomStorage MemoryRoom m where
  insertStanza (MemoryRoom ref) s = liftIO $ atomicModifyIORef' ref $ \st ->
    let key = storedKey s
     in if M.member key (mrsByKey st)
          then (st, ())
          else
            let pos = (storedTimestamp s, mrsNextSeq st)
                st' =
                  st
                    { mrsByKey = M.insert key pos (mrsByKey st)
                    , mrsByOrder = M.insert pos s (mrsByOrder st)
                    , mrsNextSeq = mrsNextSeq st + 1
                    }
             in (st', ())

  getHistory (MemoryRoom ref) q = liftIO $ do
    st <- readIORef ref
    return $ runQuery st q

runQuery :: MemRoomState -> HistoryQuery -> [StoredInStanza]
runQuery MemRoomState {mrsByKey, mrsByOrder} HistoryQuery {..} =
  applyLimit hqLimit filtered
 where
  filtered =
    [ s
    | (pos, s) <- M.toAscList mrsByOrder
    , keepTimestamp (fst pos)
    , keepId pos
    ]

  keepTimestamp t =
    checkLower hqTimeLower t && checkUpper hqTimeUpper t

  keepId pos =
    checkIdLower hqIdLower pos && checkIdUpper hqIdUpper pos

  checkIdLower Nothing _ = True
  checkIdLower (Just (inc, iid)) pos = case resolveId iid of
    Nothing -> False
    Just ref -> lowerOK inc ref pos
  checkIdUpper Nothing _ = True
  checkIdUpper (Just (inc, iid)) pos = case resolveId iid of
    Nothing -> False
    Just ref -> upperOK inc ref pos

  resolveId :: Text -> Maybe Position
  resolveId iid =
    listToMaybe
      [pos | ((_, keyId), pos) <- M.toAscList mrsByKey, keyId == iid]

checkLower :: (Ord a) => Maybe (Inclusivity, a) -> a -> Bool
checkLower Nothing _ = True
checkLower (Just (inc, b)) x = lowerOK inc b x

checkUpper :: (Ord a) => Maybe (Inclusivity, a) -> a -> Bool
checkUpper Nothing _ = True
checkUpper (Just (inc, b)) x = upperOK inc b x

lowerOK :: (Ord a) => Inclusivity -> a -> a -> Bool
lowerOK Inclusive b x = b <= x
lowerOK Exclusive b x = b < x

upperOK :: (Ord a) => Inclusivity -> a -> a -> Bool
upperOK Inclusive b x = b >= x
upperOK Exclusive b x = b > x

applyLimit :: Maybe HistoryLimit -> [a] -> [a]
applyLimit Nothing xs = xs
applyLimit (Just (FirstN n)) xs = take n xs
applyLimit (Just (LastN n)) xs = reverse (take n (reverse xs))
