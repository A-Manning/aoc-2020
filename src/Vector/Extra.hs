module Vector.Extra where

import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector

import Data.Vector (Vector)

import Function

type T a = Vector a

at :: Vector a -> Int -> a
at = (Vector.!)

atMaybe :: Vector a -> Int -> Maybe a
atMaybe = (Vector.!?)

-- returns a vector of all the `Just` values.
catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = Vector.map Maybe.fromJust . Vector.filter Maybe.isJust

-- returns the prefix of `Just` values.
takeJust :: Vector (Maybe a) -> Vector a
takeJust = catMaybes . Vector.takeWhile Maybe.isJust

-- returns the mappings for which the result is `Just`.
mapJust :: (a -> Maybe b) -> Vector a -> Vector b
mapJust = Vector.map >>$ catMaybes

-- returns the prefix of elements for which the mapping returns `Just`.
mapWhile :: (a -> Maybe b) -> Vector a -> Vector b
mapWhile = Vector.map >>$ takeJust
