{-# LANGUAGE Strict #-}

-- | Per-address disco entity cache keyed by 'DiscoNode'.
module Network.XMPP.XEP.Disco.NodeCache (
  DiscoNodeCache,
  newDiscoNodeCache,
  getDiscoNodeCache,
) where

import Control.AsyncFuture (AsyncFuture)
import qualified Control.AsyncFuture as AsyncFuture
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

newDiscoNodeCache :: (MonadStream m) => m (DiscoNodeCache m')
newDiscoNodeCache = DiscoNodeCache <$> newIORef M.empty

{- | Resolve a disco lookup against a 'DiscoNodeCache'. On cache miss, kicks
off a 'getDiscoEntityNoCache' fetch and stores its future so concurrent and
later requests for the same node share it.
-}
getDiscoNodeCache :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> DiscoNodeCache m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getDiscoNodeCache dp (DiscoNodeCache ref) addr node handler = do
  resolver <- AsyncFuture.new
  let candidate = AsyncFuture.future resolver
  (fut, mResolver) <- atomicModifyIORef ref $ \m ->
    case M.lookup node m of
      Just existing -> (m, (existing, Nothing))
      Nothing -> (M.insert node candidate m, (candidate, Just resolver))
  AsyncFuture.get fut handler
  case mResolver of
    Just r -> getDiscoEntityNoCache dp addr node $ AsyncFuture.resolve r
    Nothing -> return ()
