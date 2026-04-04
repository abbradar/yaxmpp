{-# LANGUAGE Strict #-}

module Control.Slot (
  module Data.RefMap,
  Slot,
  SlotRef,
  call,
) where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Data.String.Interpolate (i)
import UnliftIO.Exception (catch)

import Data.RefMap

type Slot m a = RefMap (a -> m ())
type SlotRef m a = RefMapRef (a -> m ())

call :: (MonadLogger m, MonadUnliftIO m) => Slot m a -> a -> m ()
call r a = do
  callbacks <- entries r
  forM_ callbacks $ \handler ->
    handler a `catch` \(e :: SomeException) -> $(logError) [i|Unhandled exception in handler: #{e}|]
