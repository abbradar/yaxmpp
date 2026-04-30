{-# LANGUAGE Strict #-}

module Control.AsyncMemo (
  AsyncMemo,
  new,
  get,
  tryGet,
)
where

import Control.Monad.IO.Unlift
import UnliftIO.Exception (mask)
import UnliftIO.IORef

data MemoState m a
  = -- | Not yet started; holds the action.
    NotStarted ((a -> m ()) -> m ())
  | -- | In progress; holds queued callbacks.
    Pending [(a -> m ())]
  | -- | Completed; holds the memoized result.
    Done a

newtype AsyncMemo m a = AsyncMemo (IORef (MemoState m a))

-- | Create a new AsyncMemo with the given callback-based action.
new :: (MonadUnliftIO m) => ((a -> m ()) -> m ()) -> m (AsyncMemo m a)
new action = AsyncMemo <$> newIORef (NotStarted action)

{- | Get the memoized result. If the action hasn't started yet, starts it
and calls the handler when done. If already done, calls the handler
immediately. If in progress, queues the handler.
-}
get :: (MonadUnliftIO m) => AsyncMemo m a -> (a -> m ()) -> m ()
get (AsyncMemo ref) handler = do
  state <- readIORef ref
  case state of
    Done a -> handler a
    _ -> mask $ \unmask -> do
      oldVal <- atomicModifyIORef ref $ \oldVal ->
        let newVal =
              case oldVal of
                Done a -> Done a
                Pending handlers -> Pending (handler : handlers)
                NotStarted _ -> Pending [handler]
         in (newVal, oldVal)
      case oldVal of
        Done a -> unmask $ handler a
        Pending _ -> return ()
        NotStarted action -> action $ \a -> do
          handlers <- atomicModifyIORef ref $ \case
            Pending handlers -> (Done a, handlers)
            _ -> error "AsyncMemo: unexpected state"
          mapM_ ($ a) $ reverse handlers

{- | Non-blocking poll. Returns 'Just' the memoized value if the action has
already completed, 'Nothing' if it has not yet started or is in progress.
Does NOT start the action.
-}
tryGet :: (MonadUnliftIO m) => AsyncMemo m a -> m (Maybe a)
tryGet (AsyncMemo ref) = do
  state <- readIORef ref
  return $ case state of
    Done a -> Just a
    _ -> Nothing
