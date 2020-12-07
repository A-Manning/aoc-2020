{-# LANGUAGE NamedFieldPuns #-}
module Solutions.Day02 where

import Foldable (count, countBy)
import Input (list)
import List ((!!?), splitOn)

data Row = Row {lo :: Int, hi :: Int, ch :: Char, pass :: String}

readRow :: String -> Row
readRow s =
  let [policy, pass] = splitOn ": " s in
  let [range, [ch]] = splitOn " " policy in
  let [lo, hi] = read <$> splitOn "-" range in
  Row {lo, hi, ch, pass}

part1 :: Row -> Bool
part1 (Row {lo, hi, ch, pass}) =
  let c = count ch pass in
  lo <= c && c <= hi

part2 :: Row -> Bool
part2 (Row {lo, hi, ch, pass}) =
  count (Just ch) [pass !!? pred lo, pass !!? pred hi] == 1

solve :: String -> IO ()
solve = go . list readRow where
  go rows = do
    print $ countBy part1 rows
    print $ countBy part2 rows

solveBB :: String -> IO ()
solveBB = solve
