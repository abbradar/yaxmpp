module Control.Signal
  ( Signal
  , empty
  , subscribe
  , emit
  ) where

import Control.Concurrent.MVar.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control

newtype Signal m a = Signal (MVar [a -> m ()])

empty :: MonadBase IO m => m (Signal m a)
empty = Signal <$> newMVar []

subscribe :: MonadBaseControl IO m => Signal m a -> (a -> m ()) -> m ()
subscribe (Signal mv) handler = modifyMVar_ mv $ return . (handler :)

emit :: MonadBase IO m => Signal m a -> a -> m ()
emit (Signal mv) val = do
  handlers <- readMVar mv
  mapM_ ($ val) handlers
