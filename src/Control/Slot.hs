{-# LANGUAGE Strict #-}

module Control.Slot
  ( module Data.RefMap
  , Slot
  , SlotRef
  , call
  ) where

import Control.Monad
import Control.Exception (SomeException)
import UnliftIO.Exception (catch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Data.String.Interpolate (i)

import Data.RefMap

type Slot m a = RefMap (a -> m ())
type SlotRef m a = RefMapRef (a -> m ())

call :: (MonadLogger m, MonadUnliftIO m) => Slot m a -> a -> m ()
call r a = do
  callbacks <- entries r
  forM_ callbacks $ \handler ->
    handler a `catch` \(e :: SomeException) -> $(logError) [i|Unhandled exception in handler: #{e}|]
