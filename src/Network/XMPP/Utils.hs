module Network.XMPP.Utils where

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
