{-# LANGUAGE Strict #-}

-- | Bidirectional monadic codecs for encoding/decoding values.
module Control.Codec (
  Codec (..),
  CodecList,
  newCodecList,
  addCodec,
  decodeAll,
  encodeAll,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.RefMap (EntryId, RefMap)
import qualified Data.RefMap as RefMap

{- | A bidirectional monadic codec between a container @value@ and a structured type @a@.

@decode@ transforms the @value@ by parsing raw data into @a@ and storing it.
@encode@ transforms the @value@ by serializing @a@ back into raw data.
-}
class Codec m value a where
  codecDecode :: a -> value -> m value
  codecEncode :: a -> value -> m value

-- | A pair of decode/encode functions, existentially hiding the codec type.
data CodecEntry m value = CodecEntry
  { entryDecode :: value -> m value
  , entryEncode :: value -> m value
  }

-- | A mutable list of codecs operating on the same @value@ type.
type CodecList m value = RefMap (CodecEntry m value)

newCodecList :: (MonadIO m) => m (CodecList m value)
newCodecList = RefMap.new

-- | Add a codec to the list, given a default value of the codec's structured type.
addCodec :: (MonadIO m, Codec m value a) => a -> CodecList m value -> m EntryId
addCodec def codecs = RefMap.add codecs $ CodecEntry (codecDecode def) (codecEncode def)

-- | Run all decoders in order, threading the value through each.
decodeAll :: (MonadIO m) => CodecList m value -> value -> m value
decodeAll codecs val = do
  es <- RefMap.entries codecs
  foldM (\v e -> entryDecode e v) val es

-- | Run all encoders in order, threading the value through each.
encodeAll :: (MonadIO m) => CodecList m value -> value -> m value
encodeAll codecs val = do
  es <- RefMap.entries codecs
  foldM (\v e -> entryEncode e v) val es
