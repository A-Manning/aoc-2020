module Solutions.Day06 where

import qualified Set

import Control.Monad (join)
import Input hiding (get)
import List (splitOn)

part1 :: [String] -> Int
part1 = Set.size . Set.fromList . join

part2 :: [String] -> Int
part2 = Set.size . foldr1 Set.intersection . map Set.fromList

solve :: String -> IO ()
solve = go . splitOn [""] . list id where
  go groups = do
    print $ sum $ map part1 groups
    print $ sum $ map part2 groups
