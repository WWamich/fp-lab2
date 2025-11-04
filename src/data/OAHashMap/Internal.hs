
module Data.OAHashMap.Internal
  ( 
    Slot (..),
    OADict (..),
    empty,
    lookup,
    initialCapacity
  )
where

import Data.Hashable (Hashable, hash)
import qualified Data.Vector as V
import Prelude hiding (lookup)

data Slot k v
  = Empty
  | Deleted
  | Occupied k v
  deriving (Show, Eq)

data OADict k v = OADict
  { dictSize :: Int,
    dictSlots :: V.Vector (Slot k v)
  }
  deriving (Show, Eq)

initialCapacity :: Int
initialCapacity = 16

empty :: OADict k v
empty = OADict 0 (V.replicate initialCapacity Empty)

lookup :: (Eq k, Hashable k) => k -> OADict k v -> Maybe v
lookup key dict
  | dictSize dict == 0 = Nothing
  | otherwise =
      let slots = dictSlots dict
          capacity = V.length slots
          startIndex = hash key `mod` capacity
          go i = case slots V.! i of
            Empty -> Nothing
            Deleted -> go ((i + 1) `mod` capacity)
            Occupied k v
              | k == key -> Just v
              | otherwise -> go ((i + 1) `mod` capacity)
       in go startIndex