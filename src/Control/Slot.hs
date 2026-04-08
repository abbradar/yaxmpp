{-# LANGUAGE Strict #-}

module Control.Slot (
  module Data.ClassVector.Mutable,
  SlotSignal (..),
  Slot,
  call,
) where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Data.ClassVector (ClassBox (..))
import Data.ClassVector.Mutable
import Data.String.Interpolate (i)
import UnliftIO.Exception (catch)

class SlotSignal m a s where
  emitSignal :: s -> a -> m ()

type Slot m a = ClassVectorRef (SlotSignal m a)

call :: (MonadLogger m, MonadUnliftIO m) => Slot m a -> a -> m ()
call slot a = do
  signals <- toAscList slot
  forM_ signals $ \(ClassBox s) ->
    emitSignal s a `catch` \(e :: SomeException) -> $(logError) [i|Unhandled exception in slot handler: #{e}|]
