module Control.Handler
  ( Handler
  , new
  , reset
  , set
  , call
  ) where

import Data.IORef.Lifted
import Control.Monad.Base

newtype Handler m a = Handler (IORef (a -> m ()))

new :: MonadBase IO m => m (Handler m a)
new = Handler <$> newIORef (\_ -> return ())

reset :: MonadBase IO m => Handler m a -> m ()
reset (Handler h) = writeIORef h $ \_ -> return ()

set :: MonadBase IO m => Handler m a -> (a -> m ()) -> m ()
set (Handler h) = writeIORef h

call :: MonadBase IO m => Handler m a -> a -> m ()
call (Handler h) a = do
  f <- readIORef h
  f a
