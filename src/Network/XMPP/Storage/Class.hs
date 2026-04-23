{-# LANGUAGE Strict #-}

{- | Abstraction for keyed, per-peer message-history storage.

A 'MessageStorage' is a map from 'XMPPAddress' to per-peer 'RoomStorage'
instances. Each room is an ordered-by-timestamp stream of 'StoredInStanza'
values, deduplicated by ('StanzaKind', id). Callers insert stanzas as they
arrive (from real-time delivery, MAM backfill, carbons, etc.) and later
query history with combinations of timestamp and id bounds.
-}
module Network.XMPP.Storage.Class (
  StoredInStanza (..),
  storedFromInStanza,
  StanzaKind (..),
  StanzaKey (..),
  stanzaKeyOf,
  Inclusivity (..),
  HistoryLimit (..),
  HistoryQuery (..),
  emptyHistoryQuery,
  MessageStorage (..),
  RoomStorage (..),
) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Network.XMPP.Address
import Network.XMPP.Stanza

{- | A stanza paired with an id and a server-authoritative timestamp. Only
stanzas with an @id@ attribute can be stored — the id is the stream key.
-}
data StoredInStanza = StoredInStanza
  { storedStanza :: InStanza
  , storedId :: Text
  , storedTimestamp :: UTCTime
  }
  deriving (Show, Eq)

-- | Wrap an 'InStanza' with a timestamp, provided the stanza carries an id.
storedFromInStanza :: UTCTime -> InStanza -> Maybe StoredInStanza
storedFromInStanza ts st = do
  sid <- istId st
  Just StoredInStanza {storedStanza = st, storedId = sid, storedTimestamp = ts}

{- | Coarse classification of a stanza. Part of the dedup key so ids
colliding across kinds (e.g. a chat reply and a presence subscription
using the same id) don't overwrite each other.
-}
data StanzaKind
  = KindMessage MessageType
  | KindPresence (Maybe PresenceType)
  deriving (Show, Eq, Ord)

data StanzaKey = StanzaKey
  { stanzaKeyKind :: StanzaKind
  , stanzaKeyId :: Text
  }
  deriving (Show, Eq, Ord)

stanzaKeyOf :: StoredInStanza -> StanzaKey
stanzaKeyOf StoredInStanza {storedStanza = InStanza {istType}, storedId} =
  StanzaKey {stanzaKeyKind = kind, stanzaKeyId = storedId}
 where
  kind = case istType of
    InMessage t -> KindMessage t
    InPresence t -> KindPresence t

data Inclusivity = Inclusive | Exclusive
  deriving (Show, Eq, Ord)

data HistoryLimit
  = FirstN Int
  | LastN Int
  deriving (Show, Eq)

{- | Intersection (AND) of optional bounds. Both timestamp and id bounds
select the same total order: entries are sorted by timestamp, ties broken
by insertion order. An id bound @>= X@ means "all entries at or after
X in the stream"; if the referenced id is not stored, no entries match.
-}
data HistoryQuery = HistoryQuery
  { hqTimeLower :: Maybe (Inclusivity, UTCTime)
  , hqTimeUpper :: Maybe (Inclusivity, UTCTime)
  , hqIdLower :: Maybe (Inclusivity, Text)
  , hqIdUpper :: Maybe (Inclusivity, Text)
  , hqLimit :: Maybe HistoryLimit
  }
  deriving (Show, Eq)

emptyHistoryQuery :: HistoryQuery
emptyHistoryQuery =
  HistoryQuery
    { hqTimeLower = Nothing
    , hqTimeUpper = Nothing
    , hqIdLower = Nothing
    , hqIdUpper = Nothing
    , hqLimit = Nothing
    }

class (Monad m) => MessageStorage s m where
  type MessageStorageRoom s
  getOrCreateRoom :: s -> XMPPAddress -> m (MessageStorageRoom s)

class (Monad m) => RoomStorage r m where
  -- | Insert a stanza. Silently no-ops if an entry with the same
  -- ('StanzaKind', id) is already present.
  insertStanza :: r -> StoredInStanza -> m ()

  -- | Return entries satisfying the query, ordered by ('storedTimestamp',
  -- insertion-order) ascending.
  getHistory :: r -> HistoryQuery -> m [StoredInStanza]
