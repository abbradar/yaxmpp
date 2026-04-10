{-# LANGUAGE Strict #-}

module Control.LazyOnce (
  LazyOnce,
  new,
  get,
) where

import Control.Monad.IO.Unlift
import UnliftIO.Exception
import UnliftIO.MVar

newtype LazyOnce m a = LazyOnce (MVar (Either (m a) a))

new :: (MonadUnliftIO m) => m a -> m (LazyOnce m a)
new action = LazyOnce <$> newMVar (Left action)

get :: (MonadUnliftIO m) => LazyOnce m a -> m a
get lazy@(LazyOnce var) = do
  oldValue <- readMVar var
  case oldValue of
    Right a -> return a
    Left _ -> do
      result <- mask $ \restore -> do
        took <- tryTakeMVar var
        case took of
          Nothing -> return Nothing
          Just value@(Right a) -> do
            putMVar var value
            return $ Just a
          Just value@(Left action) -> do
            a <- restore action `onException` putMVar var value
            putMVar var (Right a)
            return $ Just a
      case result of
        Just a -> return a
        Nothing -> get lazy
