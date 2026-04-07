{-# LANGUAGE Strict #-}

-- | Bidirectional monadic codecs for encoding/decoding values.
module Control.Codec (
  Codec (..),
  CodecRef (..),
  CodecList,
  newCodecList,
  addCodec,
  decodeAll,
  encodeAll,
)
where

import Control.Monad
import Control.Monad.IO.Class
import Data.RefMap (EntryId, RefMap)
import qualified Data.RefMap as RefMap

{- | A bidirectional monadic codec between a container @value@ and a structured type @a@.

@codecDecode@ transforms the @value@ by parsing raw data into @a@ and storing it.
@codecEncode@ transforms the @value@ by serializing @a@ back into raw data.
-}
class Codec m value a where
  codecDecode :: a -> value -> m value
  codecEncode :: a -> value -> m value

-- | An existential wrapper around a codec value.
data CodecRef m value = forall a. (Codec m value a) => CodecRef a

-- | A mutable list of codecs operating on the same @value@ type.
type CodecList m value = RefMap (CodecRef m value)

newCodecList :: (MonadIO m) => m (CodecList m value)
newCodecList = RefMap.new

-- | Add a codec to the list.
addCodec :: (MonadIO m, Codec m value a) => a -> CodecList m value -> m EntryId
addCodec v codecs = RefMap.add codecs (CodecRef v)

-- | Run all decoders in order, threading the value through each.
decodeAll :: (MonadIO m) => CodecList m value -> value -> m value
decodeAll codecs val = do
  es <- RefMap.entries codecs
  foldM (\v (CodecRef c) -> codecDecode c v) val es

-- | Run all encoders in reverse order, threading the value through each.
encodeAll :: (MonadIO m) => CodecList m value -> value -> m value
encodeAll codecs val = do
  es <- RefMap.entries codecs
  foldM (\v (CodecRef c) -> codecEncode c v) val es
