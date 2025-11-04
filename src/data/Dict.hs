
module Data.Dict
  ( 
    OADict,
    empty,
    lookup,
    toList,
    insert,
    delete
  )
where

import Prelude hiding (lookup)
import Data.OAHashMap.Internal
  ( OADict,
    empty,
    lookup,
    toList,
    insert,
    delete
  )