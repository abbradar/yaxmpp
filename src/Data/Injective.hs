module Data.Injective where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types

class (Enum a, Bounded a) => Injective a b where
  injTo :: a -> b

injFrom :: (Injective a b, Ord b) => b -> Maybe a
injFrom val = M.lookup val rev
  where rev = M.fromListWith (error "injFrom: injTo violates injectivity") $ map (\x -> (injTo x, x)) [minBound..maxBound]

injParseJSON :: Injective a Text => String -> Value -> Parser a
injParseJSON err = withText err $ \t -> case injFrom t of
  Nothing -> fail err
  Just r -> return r

injToJSON :: Injective a Text => a -> Value
injToJSON = String . injTo
