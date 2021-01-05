module Solutions.Day12 where

import qualified List

import Input hiding (get)

step1 :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
step1 (dir, x, y) ('N', amt) = (dir, x, y + amt)
step1 (dir, x, y) ('S', amt) = (dir, x, y - amt)
step1 (dir, x, y) ('E', amt) = (dir, x + amt, y)
step1 (dir, x, y) ('W', amt) = (dir, x - amt, y)
step1 (dir, x, y) ('L', amt) = ((dir - amt) `mod` 360, x, y)
step1 (dir, x, y) ('R', amt) = ((dir + amt) `mod` 360, x, y)
step1 (  0, x, y) ('F', amt) = (  0, x, y + amt)
step1 ( 90, x, y) ('F', amt) = ( 90, x + amt, y)
step1 (180, x, y) ('F', amt) = (180, x, y - amt)
step1 (270, x, y) ('F', amt) = (270, x - amt, y)

step2 :: (Int, Int, Int, Int) -> (Char, Int) -> (Int, Int, Int, Int)
step2 (x, y, wx, wy) ('N', amt) = (x, y, wx, wy + amt)
step2 (x, y, wx, wy) ('S', amt) = (x, y, wx, wy - amt)
step2 (x, y, wx, wy) ('E', amt) = (x, y, wx + amt, wy)
step2 (x, y, wx, wy) ('W', amt) = (x, y, wx - amt, wy)
step2 (x, y, wx, wy) ('F', amt) = (x + (wx * amt), y + (wy * amt), wx, wy)
step2 (x, y, wx, wy) ('L',  90) = (x, y, -wy,  wx)
step2 (x, y, wx, wy) ('L', 180) = (x, y, -wx, -wy)
step2 (x, y, wx, wy) ('L', 270) = (x, y,  wy, -wx)
step2 (x, y, wx, wy) ('R',  90) = (x, y,  wy, -wx)
step2 (x, y, wx, wy) ('R', 180) = (x, y, -wx, -wy)
step2 (x, y, wx, wy) ('R', 270) = (x, y, -wy,  wx)

part1 :: [(Char, Int)] -> Int
part1 instrs =
  let (_, x, y) = foldl step1 (90, 0, 0) instrs in
  abs x + abs y

part2 :: [(Char, Int)] -> Int
part2 instrs =
  let (x, y, _, _) = foldl step2 (0, 0, 10, 1) instrs in
  abs x + abs y

solve :: String -> IO ()
solve = go . list (parse . head . List.splitOn " ") where
  go instrs = do
    print $ part1 instrs
    print $ part2 instrs
  parse (c:rest) = (c, read rest)
