module Network.XMPP.Utils where

import Control.Arrow
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Fail (MonadFail)
import Data.Text (Text)
import Data.Attoparsec.Text

parseValue :: Parser a -> Text -> Maybe a
parseValue parser t = case parseOnly (parser <* endOfInput) t of
  Left _ -> Nothing
  Right res -> Just res

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
