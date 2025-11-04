
module Data.OAHashMap.Internal
  ( 
    Slot (..),
    OADict (..),
    empty,
    lookup,
    toList,
    insert,
    delete,
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

toList :: OADict k v -> [(k, v)]
toList = V.foldr toList' [] . dictSlots
  where
    toList' (Occupied k v) acc = (k, v) : acc
    toList' _ acc = acc

insert :: (Eq k, Hashable k) => k -> v -> OADict k v -> OADict k v
insert key value dict
  | needsResize dict = insert key value (resize dict)
  | otherwise =
      let slots    = dictSlots dict
          capacity = V.length slots
          startIndex = hash key `mod` capacity
          go i = case slots V.! i of
            Empty ->
              let newSlots = slots V.// [(i, Occupied key value)]
              in OADict (dictSize dict + 1) newSlots
            Deleted ->
              let newSlots = slots V.// [(i, Occupied key value)]
              in OADict (dictSize dict + 1) newSlots
            Occupied k _
              | k == key ->
                  let newSlots = slots V.// [(i, Occupied key value)]
                  in OADict (dictSize dict) newSlots
              | otherwise -> go ((i + 1) `mod` capacity)
       in go startIndex

needsResize :: OADict k v -> Bool
needsResize dict =
  let capacity = fromIntegral $ V.length (dictSlots dict)
      size = fromIntegral $ dictSize dict
  in
     size * 4 >= capacity * 3

resize :: (Eq k, Hashable k) => OADict k v -> OADict k v
resize dict =
  let oldPairs    = toList dict
      newCapacity = V.length (dictSlots dict) * 2
      newEmptyDict = OADict 0 (V.replicate newCapacity Empty)
  in foldr (\(k, v) accDict -> insert k v accDict) newEmptyDict oldPairs

delete :: (Eq k, Hashable k) => k -> OADict k v -> OADict k v
delete key dict =
  let slots = dictSlots dict
      capacity = V.length slots
      startIndex = hash key `mod` capacity
      go i = case slots V.! i of
        Empty -> dict
        Occupied k _
          | k == key ->
              let newSlots = slots V.// [(i, Deleted)]
              in OADict (dictSize dict - 1) newSlots
          | otherwise -> go ((i + 1) `mod` capacity)
        Deleted -> go ((i + 1) `mod` capacity)
  in go startIndex


