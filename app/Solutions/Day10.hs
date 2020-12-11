module Solutions.Day10 where

import qualified List
import qualified Map

import Function
import Input hiding (get)
import Map ((!))

kdiffs :: Int -> [Int] -> Int
kdiffs k = go where
  go (x0 : x1 : xs) =
    if (x1 - x0) == k then succ $ go (x1 : xs) else go (x1 : xs)
  go _ = 0

part1 :: [Int] -> IO ()
part1 nums = do
  print $ succ (kdiffs 1 nums) * succ (kdiffs 3 nums)

part2 :: [Int] -> IO ()
part2 = print . snd . go Map.empty where
  go memo [] = (memo, 0)
  go memo [x] = (Map.insert x 1 memo, 1)
  go memo (x:xs) =
    let (m, res) = take 3 xs
                   & filter (\y -> y - x <= 3)
                   & map (\y -> List.dropWhile (< y) xs)
                   & List.mapAccumR lookupMemo memo in
    (Map.insert x (sum res) m, sum res)
  lookupMemo memo [] = (memo, 0)
  lookupMemo memo (x:xs) =
    if Map.member x memo then (memo, memo ! x) else
    go memo (x:xs)

solve :: String -> IO ()
solve = go . List.sort . list (read . head . List.splitOn " ") where
  go nums = do
    part1 nums
    part2 (0:nums)
