{-# LANGUAGE Strict #-}

module Control.MemoAsync (
  MemoAsync,
  new,
  get,
)
where

import Control.Monad.IO.Unlift
import UnliftIO.IORef

data MemoState m a
  = -- | Not yet started; holds the action.
    NotStarted ((a -> m ()) -> m ())
  | -- | In progress; holds queued callbacks.
    InProgress [(a -> m ())]
  | -- | Completed; holds the memoized result.
    Done a

newtype MemoAsync m a = MemoAsync (IORef (MemoState m a))

-- | Create a new MemoAsync with the given callback-based action.
new :: (MonadUnliftIO m) => ((a -> m ()) -> m ()) -> m (MemoAsync m a)
new action = MemoAsync <$> newIORef (NotStarted action)

{- | Get the memoized result. If the action hasn't started yet, starts it
and calls the handler when done. If already done, calls the handler
immediately. If in progress, queues the handler.
-}
get :: (MonadUnliftIO m) => MemoAsync m a -> (a -> m ()) -> m ()
get (MemoAsync ref) handler = do
  state <- readIORef ref
  case state of
    Done a -> handler a
    _ -> do
      oldVal <- atomicModifyIORef ref $ \oldVal ->
        let newVal =
              case oldVal of
                Done a -> Done a
                InProgress handlers -> InProgress (handler : handlers)
                NotStarted _ -> InProgress [handler]
         in (newVal, oldVal)
      case oldVal of
        Done a -> handler a
        InProgress _ -> return ()
        NotStarted action -> action $ \a -> do
          handlers <- atomicModifyIORef ref $ \case
            InProgress handlers -> (Done a, handlers)
            _ -> error "MemoAsync: unexpected state"
          mapM_ ($ a) $ reverse handlers
