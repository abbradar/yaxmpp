{-# LANGUAGE Strict #-}

module Control.HandlerList (
  module Data.ClassVector.Mutable,
  Handler (..),
  HandlerList,
  call,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ClassVector (ClassBox (..))
import Data.ClassVector.Mutable

class Handler m a b h where
  tryHandle :: h -> a -> m (Maybe b)

type HandlerList m a b = ClassVectorRef (Handler m a b)

call :: (MonadUnliftIO m) => HandlerList m a b -> a -> m (Maybe b)
call hl arg = do
  handlers <- toAscList hl
  runUntil handlers arg

runUntil :: (Monad m) => [ClassBox (Handler m a b)] -> a -> m (Maybe b)
runUntil [] _ = return Nothing
runUntil (ClassBox h : hs) arg = do
  mr <- tryHandle h arg
  case mr of
    Nothing -> runUntil hs arg
    Just r -> return $ Just r
