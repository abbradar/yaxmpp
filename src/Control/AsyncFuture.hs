{-# LANGUAGE Strict #-}

module Control.AsyncFuture (
  AsyncFuture,
  AsyncFutureResolver,
  new,
  resolve,
  future,
  get,
)
where

import Control.Monad.IO.Unlift
import UnliftIO.IORef

data FutureState m a
  = Pending [a -> m ()]
  | Done a

newtype AsyncFuture m a = AsyncFuture (IORef (FutureState m a))

newtype AsyncFutureResolver m a = AsyncFutureResolver (IORef (FutureState m a))

{- | Create a new unresolved future. Returns the resolver; call 'future'
to obtain the read-only handle to share with consumers.
-}
new :: (MonadUnliftIO m) => m (AsyncFutureResolver m a)
new = AsyncFutureResolver <$> newIORef (Pending [])

{- | Read-only handle for consumers. Hand this out instead of the resolver
to keep resolution authority on the producer side.
-}
future :: AsyncFutureResolver m a -> AsyncFuture m a
future (AsyncFutureResolver ref) = AsyncFuture ref

{- | Resolve the future with the given value, running all queued handlers
in-place in registration order. Subsequent calls are a no-op (the first
resolution wins).
-}
resolve :: (MonadUnliftIO m) => AsyncFutureResolver m a -> a -> m ()
resolve (AsyncFutureResolver ref) a = do
  handlers <- atomicModifyIORef ref $ \case
    Pending hs -> (Done a, hs)
    Done a' -> (Done a', [])
  mapM_ ($ a) $ reverse handlers

{- | Get the resolved value. If already resolved, calls the handler
immediately; otherwise queues it to run on the next 'resolve'.
-}
get :: (MonadUnliftIO m) => AsyncFuture m a -> (a -> m ()) -> m ()
get (AsyncFuture ref) handler = do
  state <- readIORef ref
  case state of
    Done a -> handler a
    Pending _ -> do
      oldVal <- atomicModifyIORef ref $ \oldVal ->
        case oldVal of
          Done a -> (Done a, oldVal)
          Pending hs -> (Pending (handler : hs), oldVal)
      case oldVal of
        Done a -> handler a
        Pending _ -> return ()
