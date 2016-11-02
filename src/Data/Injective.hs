module Data.Injective where

import qualified Data.Map.Strict as M

class (Enum a, Bounded a) => Injective a b where
  injTo :: a -> b

injFrom :: (Injective a b, Ord b) => b -> Maybe a
injFrom val = M.lookup val rev
  where rev = M.fromListWith (error "injFrom: injTo violates injectivity") $ map (\x -> (injTo x, x)) [minBound..maxBound]
