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
  Inclusivity (..),
  HistoryLimit (..),
  HistoryQuery (..),
  MessageStorage (..),
  RoomStorage (..),
) where

import Control.Applicative ((<|>))
import Data.Time (UTCTime)

import Network.XMPP.Address
import Network.XMPP.Stanza

data StorageId
  = NoStorageId
  | OurStoredId StanzaId
  | MAMStoredId StanzaId
  deriving (Show, Eq)

{- | A stanza paired with an id and a server-authoritative timestamp. Only
stanzas with an @id@ attribute can be stored — the id is the stream key.
-}
data StoredInStanza = StoredInStanza
  { storedStanza :: InStanza
  , -- We index on (`storedFrom`, `storedStanzaId`, `storedInitialTimestamp`) to allow related stanzas correlation.
    -- such as Last Message Correction.
    storedFrom :: XMPPAddress
  , storedStanzaId :: StanzaId
  , -- This timestamp is used as a tie-breaker in indexes and is the first timestamp we see;
    -- for the received messages without <delay> and for the sent messages it's the locally
    -- generated timestamp. We also order by it for
    storedInitialTimestamp :: UTCTime
  , -- We index on (`storedStorageId`, `storedInitialTimestamp`) to allow deduplication.
    storedStorageId :: StorageId
  , -- We order by `storedTimestamp`; it may be updated when we sync with the archive.
    storedTimestamp :: UTCTime
  }
  deriving (Show, Eq)

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

Per-field right-biased 'Semigroup': @a <> b@ takes each field from @b@ if
set, otherwise from @a@. Useful for layering partial queries on top of a
base, e.g. @base \<\> mempty { hqLimit = Just (LastN 50) }@.
-}
data HistoryQuery = HistoryQuery
  { hqTimeLower :: Maybe (Inclusivity, UTCTime)
  , hqTimeUpper :: Maybe (Inclusivity, UTCTime)
  , hqIdLower :: Maybe (Inclusivity, StanzaId)
  , hqIdUpper :: Maybe (Inclusivity, StanzaId)
  , hqLimit :: Maybe HistoryLimit
  }
  deriving (Show, Eq)

instance Semigroup HistoryQuery where
  a <> b =
    HistoryQuery
      { hqTimeLower = hqTimeLower b <|> hqTimeLower a
      , hqTimeUpper = hqTimeUpper b <|> hqTimeUpper a
      , hqIdLower = hqIdLower b <|> hqIdLower a
      , hqIdUpper = hqIdUpper b <|> hqIdUpper a
      , hqLimit = hqLimit b <|> hqLimit a
      }

instance Monoid HistoryQuery where
  mempty =
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
  {- | Insert a stanza. Silently no-ops if an entry with the same
  ('StanzaKind', id) is already present.
  -}
  insertStanza :: r -> StoredInStanza -> m ()

  {- | Return entries satisfying the query, ordered by ('storedTimestamp',
  insertion-order) ascending.
  -}
  getHistory :: r -> HistoryQuery -> m [StoredInStanza]
