module Control.Handler
  ( Handler
  , new
  , reset
  , set
  , call
  ) where

import UnliftIO.IORef
import Control.Monad.IO.Class

newtype Handler m a = Handler (IORef (a -> m ()))

new :: MonadIO m => m (Handler m a)
new = Handler <$> newIORef (\_ -> return ())

reset :: MonadIO m => Handler m a -> m ()
reset (Handler h) = writeIORef h $ \_ -> return ()

set :: MonadIO m => Handler m a -> (a -> m ()) -> m ()
set (Handler h) = writeIORef h

call :: MonadIO m => Handler m a -> a -> m ()
call (Handler h) a = do
  f <- readIORef h
  f a
