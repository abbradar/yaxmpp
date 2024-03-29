module Network.XMPP.Utils where

import Control.Arrow
import Text.Read
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Attoparsec.Text

parseValue :: Parser a -> Text -> Either String a
parseValue parser = parseOnly (parser <* endOfInput)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def Nothing = Left def
maybeToEither _ (Just val) = Right val

maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail _ (Just a) = return a
maybeFail err Nothing = fail err

mapDisjointUnion :: Ord k => Map k a -> Map k a -> Maybe (Map k a)
mapDisjointUnion a b = do
  let r = M.union a b
  when (M.size r /= M.size a + M.size b) $ fail "mapDisjointUnion: shared items"
  return r

setDisjointUnion :: Ord a => Set a -> Set a -> Maybe (Set a)
setDisjointUnion a b = do
  let r = S.union a b
  when (S.size r /= S.size a + S.size b) $ fail "setDisjointUnion: shared items"
  return r

mapDisjointFromList :: Ord k => [(k, a)] -> Maybe (Map k a)
mapDisjointFromList = sequence . M.fromListWith conflict . fmap (second return)
  where conflict _ _ = fail "mapDisjointFromList: shared items"

toRight :: Either a b -> Maybe b
toRight (Right r) = Just r
toRight _ = Nothing

readIntMaybe :: forall a. (Bounded a, Integral a) => String -> Maybe a
readIntMaybe str = do
  (int :: Integer) <- readMaybe str
  when (int < fromIntegral (minBound :: a)) $ fail "readIntMaybe: too small"
  when (int > fromIntegral (maxBound :: a)) $ fail "readIntMaybe: too big"
  return $ fromIntegral int
