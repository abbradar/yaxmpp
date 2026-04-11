{-# LANGUAGE Strict #-}

module Control.Concurrent.Linked (
  forkLinked,
) where

import Control.Exception (AsyncException (..), fromException)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (ThreadId, forkFinally, myThreadId)
import UnliftIO.Exception (throwTo)

{- | Fork a thread that is linked to the current thread: if the forked
thread terminates with an exception, that exception is re-thrown in
the parent thread. Normal termination and 'ThreadKilled' are ignored.
-}
forkLinked :: (MonadUnliftIO m) => m () -> m ThreadId
forkLinked action = do
  parent <- myThreadId
  forkFinally action $ \case
    Right () -> return ()
    Left e
      | Just ThreadKilled <- fromException e -> return ()
      | otherwise -> throwTo parent e
