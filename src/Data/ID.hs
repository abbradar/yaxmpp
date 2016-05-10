module Data.ID
       ( IDGen
       , empty
       , get
       , free
       ) where

import Data.Set (Set)
import qualified Data.Set as S

data IDGen = IDGen { lastId :: Integer
                   , freeIds :: Set Integer
                   }

empty :: IDGen
empty = IDGen { lastId = 0
              , freeIds = S.empty
              }

get :: IDGen -> (IDGen, Integer)
get gen@IDGen {..}
  | S.null freeIds = (gen { lastId = succ lastId }, lastId)
  | otherwise = let m = S.findMin freeIds
                in (gen { freeIds = S.delete m freeIds }, m)

free :: Integer -> IDGen -> IDGen
free n0 gen0 = optimize $ gen0 { freeIds = S.insert n0 $ freeIds gen0 }
  where optimize gen@IDGen {..} =
          let n = pred lastId
          in if n `S.member` freeIds
             then optimize IDGen { lastId = n
                                 , freeIds = S.delete n freeIds
                                 }
             else gen
