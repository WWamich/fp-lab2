
module Data.Dict
  ( 
    OADict,
    empty,
    lookup,
    toList,
    insert,
    delete,
    fromList,
    mapDict,
    filterDict,
    foldrDict,
    foldlDict
  )
where

import Prelude hiding (lookup)
import Data.OAHashMap.Internal
  ( OADict,
    empty,
    lookup,
    toList,
    insert,
    delete,
    fromList,
    mapDict,
    filterDict,
    foldrDict,
    foldlDict
  )