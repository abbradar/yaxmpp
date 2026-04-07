{-# LANGUAGE Strict #-}

module Control.HandlerList (
  module Data.RefMap,
  HandlerList,
  call,
)
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.RefMap

type HandlerList m a b = RefMap (a -> m (Maybe b))

runUntil :: (Monad m) => [a -> m (Maybe b)] -> a -> m (Maybe b)
runUntil [] _ = return Nothing
runUntil (h : hs) arg = do
  mr <- h arg
  case mr of
    Nothing -> runUntil hs arg
    Just r -> return $ Just r

call :: (MonadUnliftIO m) => HandlerList m a b -> a -> m (Maybe b)
call r arg = do
  callbacks <- entries r
  runUntil callbacks arg
