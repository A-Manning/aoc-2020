module Map.Extra where

import qualified Data.Map.Strict as Map

import Data.Map.Strict (Map)

type T key value = Map key value

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) = flip Map.lookup
