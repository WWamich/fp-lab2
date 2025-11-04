module Dict
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
    foldlDict,
  )
where

import Data.Dict.Internal
  ( OADict,
    delete,
    empty,
    filterDict,
    foldlDict,
    foldrDict,
    fromList,
    insert,
    lookup,
    mapDict,
    toList,
  )
import Prelude hiding (lookup)
