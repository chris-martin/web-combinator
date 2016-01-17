module WebCombinator.Util
    ( mapEntries
    , mapEntriesMaybe
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe (mapMaybe)
import Prelude ((.), map, Ord)

mapEntries :: (Ord k1, Ord k2) => ((k1, a1) -> (k2, a2)) -> Map k1 a1 -> Map k2 a2
mapEntries f = Map.fromList . (map f) . Map.toList

mapEntriesMaybe :: (Ord k1, Ord k2) => ((k1, a1) -> Maybe (k2, a2)) -> Map k1 a1 -> Map k2 a2
mapEntriesMaybe f = Map.fromList . (mapMaybe f) . Map.toList
