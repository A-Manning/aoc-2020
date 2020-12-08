{-# LANGUAGE LambdaCase #-}
module Solutions.Day05 where

import qualified Set

import Input hiding (get)
import List (maximum)
import Numeric (readInt)

decode :: String -> (Int, Int)
decode s =
  let (r, c) = splitAt 7 s in
  (go r, go c) where
    go = fst . head . readInt 2 (flip elem "BFLR")
                                (\case c | elem c "FL" -> 0 | elem c "BR" -> 1)

seatID :: (Int, Int) -> Int
seatID (r, c) = (r * 8) + c

part1 :: [(Int, Int)] -> Int
part1 = maximum . map seatID

part2 :: [(Int, Int)] -> [Int]
part2 poss =
  let ids = Set.fromList $ map seatID poss in
  filter (\e -> not $ Set.member e ids) [Set.findMin ids..part1 poss]

solve :: String -> IO ()
solve = go . list decode where
  go positions = do
    print $ part1 positions
    print $ part2 positions

solveBB :: String -> IO ()
solveBB = solve
