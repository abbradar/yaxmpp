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

@meta@ is additional context passed to the codec (e.g. the source address).
@codecDecode@ transforms the @value@ by parsing raw data into @a@ and storing it.
@codecEncode@ transforms the @value@ by serializing @a@ back into raw data.
-}
class Codec m meta value a where
  codecDecode :: a -> meta -> value -> m value
  codecEncode :: a -> meta -> value -> m value

-- | A mutable list of codecs operating on the same @value@ type.
type CodecList m meta value = ClassVectorRef (Codec m meta value)

-- | Run all decoders in order, threading the value through each.
decodeAll :: (MonadIO m) => CodecList m meta value -> meta -> value -> m value
decodeAll codecs meta val = do
  es <- toAscList codecs
  foldM (\v (ClassBox c) -> codecDecode c meta v) val es

-- | Run all encoders in reverse order, threading the value through each.
encodeAll :: (MonadIO m) => CodecList m meta value -> meta -> value -> m value
encodeAll codecs meta val = do
  es <- toDescList codecs
  foldM (\v (ClassBox c) -> codecEncode c meta v) val es
