{-# LANGUAGE Strict #-}

{- | One-way monadic filters with possible errors, used to enrich incoming or
outgoing stanza components with extension-specific metadata.
-}
module Network.XMPP.Filter (
  module Data.ClassVector.Mutable,
  FilterReceive (..),
  FilterSend (..),
  FilterReceiveList,
  FilterSendList,
  runReceiveFilters,
  runSendFilters,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ClassBox (ClassBox (..))
import Data.ClassVector.Mutable

{- | Receive-side monadic filter: transforms an incoming @value@ on its way
in. @meta@ carries additional context (e.g. the source address).
-}
class FilterReceive m meta value e a where
  filterReceive :: a -> meta -> value -> m (Either e value)

{- | Send-side monadic filter: transforms an outgoing @value@ on its way
out. @meta@ carries additional context (e.g. the destination address).
-}
class FilterSend m meta value e a where
  filterSend :: a -> meta -> value -> m (Either e value)

-- | A mutable list of receive filters operating on the same @value@/@e@ types.
type FilterReceiveList m meta e value = ClassVectorRef (FilterReceive m meta value e)

-- | A mutable list of send filters operating on the same @value@/@e@ types.
type FilterSendList m meta e value = ClassVectorRef (FilterSend m meta value e)

-- | Run all receive filters in order. Stops on the first error.
runReceiveFilters :: (MonadIO m) => FilterReceiveList m meta e value -> meta -> value -> m (Either e value)
runReceiveFilters filters meta val = do
  es <- toAscList filters
  foldM step (Right val) es
 where
  step (Left err) _ = return $ Left err
  step (Right v) (ClassBox f) = filterReceive f meta v

-- | Run all send filters in reverse order. Stops on the first error.
runSendFilters :: (MonadIO m) => FilterSendList m meta e value -> meta -> value -> m (Either e value)
runSendFilters filters meta val = do
  es <- toDescList filters
  foldM step (Right val) es
 where
  step (Left err) _ = return $ Left err
  step (Right v) (ClassBox f) = filterSend f meta v
