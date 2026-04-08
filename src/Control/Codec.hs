{-# LANGUAGE Strict #-}

-- | Bidirectional monadic codecs for encoding/decoding values.
module Control.Codec (
  module Data.ClassVector.Mutable,
  Codec (..),
  CodecList,
  decodeAll,
  encodeAll,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ClassVector (ClassBox (..))
import Data.ClassVector.Mutable

{- | A bidirectional monadic codec between a container @value@ and a structured type @a@.

@codecDecode@ transforms the @value@ by parsing raw data into @a@ and storing it.
@codecEncode@ transforms the @value@ by serializing @a@ back into raw data.
-}
class Codec m value a where
  codecDecode :: a -> value -> m value
  codecEncode :: a -> value -> m value

-- | A mutable list of codecs operating on the same @value@ type.
type CodecList m value = ClassVectorRef (Codec m value)

-- | Run all decoders in order, threading the value through each.
decodeAll :: (MonadIO m) => CodecList m value -> value -> m value
decodeAll codecs val = do
  es <- toAscList codecs
  foldM (\v (ClassBox c) -> codecDecode c v) val es

-- | Run all encoders in reverse order, threading the value through each.
encodeAll :: (MonadIO m) => CodecList m value -> value -> m value
encodeAll codecs val = do
  es <- toDescList codecs
  foldM (\v (ClassBox c) -> codecEncode c v) val es
