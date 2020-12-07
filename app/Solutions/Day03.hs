module Solutions.Day03 where

import qualified Matrix

import Function ((&))
import Input (matrix)

countTrees :: Matrix.T Char -> (Int, Int) -> Int
countTrees m (down, right) = go (0, 0) where
  go (x, y) =
    let nextPosition = (x + right, y + down) in
    case Matrix.atMaybeWrapCol m nextPosition of
      (Just '.') -> go nextPosition
      (Just '#') -> 1 + go nextPosition
      _ -> 0

solve :: String -> IO ()
solve = go . matrix where
  go arena = do
    countTrees arena (1, 3) & print

    map (countTrees arena) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
      & product & print

solveBB :: String -> IO ()
solveBB = go . matrix where
  go arena =
    let rights = [2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 32, 36, 48, 54, 64] in
    let downs = [1, 5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35, 37, 41, 47] in
    let paths = map (,) downs <*> rights in
    print $ map (countTrees arena) paths
