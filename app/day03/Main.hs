module Main where

import qualified Matrix

import Function ((&))
import Input (inputMatrix)

countTrees :: Matrix.T Char -> (Int, Int) -> Int
countTrees m (down, right) = go (0, 0) where
  go (x, y) =
    let nextPosition = (x + right, y + down) in
    case Matrix.atMaybeWrapCol m nextPosition of
      (Just '.') -> go nextPosition
      (Just '#') -> 1 + go nextPosition
      _ -> 0

main :: IO ()
main = do
  arena <- inputMatrix 03;
  countTrees arena (1, 3) & print

  map (countTrees arena) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    & product & print
