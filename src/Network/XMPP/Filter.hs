{-# LANGUAGE Strict #-}

{- | One-way monadic filters with possible errors, used to enrich incoming or
outgoing stanza components with extension-specific metadata.
-}
module Network.XMPP.Filter (
  module Data.ClassVector.Mutable,
  Filter (..),
  FilterList,
  runReceiveFilters,
  runSendFilters,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ClassBox (ClassBox (..))
import Data.ClassVector.Mutable

{- | A pair of one-way monadic conversions between a container @value@ and a
structured type @a@, both of which may fail with @e@.

@meta@ is additional context passed to the filter (e.g. the source address).
@filterReceive@ transforms an incoming @value@ on its way in.
@filterSend@ transforms an outgoing @value@ on its way out.
-}
class Filter m meta value e a where
  filterReceive :: a -> meta -> value -> m (Either e value)
  filterSend :: a -> meta -> value -> m (Either e value)

-- | A mutable list of filters operating on the same @value@/@e@ types.
type FilterList m meta e value = ClassVectorRef (Filter m meta value e)

-- | Run all receive filters in order, threading the value through each. Stops on the first error.
runReceiveFilters :: (MonadIO m) => FilterList m meta e value -> meta -> value -> m (Either e value)
runReceiveFilters filters meta val = do
  es <- toAscList filters
  foldM step (Right val) es
 where
  step (Left err) _ = return $ Left err
  step (Right v) (ClassBox f) = filterReceive f meta v

-- | Run all send filters in reverse order, threading the value through each. Stops on the first error.
runSendFilters :: (MonadIO m) => FilterList m meta e value -> meta -> value -> m (Either e value)
runSendFilters filters meta val = do
  es <- toDescList filters
  foldM step (Right val) es
 where
  step (Left err) _ = return $ Left err
  step (Right v) (ClassBox f) = filterSend f meta v
