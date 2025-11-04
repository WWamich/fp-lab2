module Dict (
    Dict 
) where 

import qualified Data.Vector as V

data Slot k v = Empty
                | Deleted
                | Occupied k v
                deriving (Show, Eq)

data Dict k v = Dict {
    slots :: V.Vector (Slot k v),
    size :: Int
} deriving (Show, Eq)