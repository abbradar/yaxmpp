{-# LANGUAGE Strict #-}

-- | Per-address disco entity cache keyed by 'DiscoNode'.
module Network.XMPP.XEP.Disco.NodeCache (
  DiscoNodeCache,
  new,
  get,
  clear,
) where

import Control.AsyncFuture (AsyncFuture)
import qualified Control.AsyncFuture as AsyncFuture
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.Exception (mask_)
import UnliftIO.IORef

import Network.XMPP.Address
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco

{- | Per-address cache of disco entity lookups, keyed by node. Lazily
populated; concurrent requests for the same node share a single in-flight
fetch via the underlying 'AsyncFuture'.
-}
newtype DiscoNodeCache m = DiscoNodeCache (IORef (Map (Maybe DiscoNode) (AsyncFuture m (Either StanzaError DiscoEntity))))

new :: (MonadIO m) => m (DiscoNodeCache m')
new = DiscoNodeCache <$> newIORef M.empty

{- | Resolve a disco lookup against a 'DiscoNodeCache'. On cache miss, kicks
off a 'getDiscoEntityNoCache' fetch and stores its future so concurrent and
later requests for the same node share it.
-}
get :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> DiscoNodeCache m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
get dp (DiscoNodeCache ref) addr node handler = do
  existing <- M.lookup node <$> readIORef ref
  fut <- case existing of
    Just fut -> return fut
    Nothing -> do
      resolver <- AsyncFuture.new
      let candidate = AsyncFuture.future resolver
      mask_ $ do
        (fut, isNew) <- atomicModifyIORef ref $ \m ->
          case M.lookup node m of
            Just other -> (m, (other, False))
            Nothing -> (M.insert node candidate m, (candidate, True))
        when isNew $ getDiscoEntityNoCache dp addr node $ AsyncFuture.resolve resolver
        return fut
  AsyncFuture.get fut handler

-- | Drop all cached entries. In-flight fetches still resolve their futures, but new requests will start fresh fetches.
clear :: (MonadIO m) => DiscoNodeCache m' -> m ()
clear (DiscoNodeCache ref) = writeIORef ref M.empty
