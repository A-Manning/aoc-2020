module Solutions.Day13 where

import qualified List
import qualified Set

import Input hiding (get)

part1 :: (Int, [String]) -> IO ()
part1 (ts, buses') =
  go ts where
  go time =
    let res = Set.filter (\e -> (time `mod` e) == 0) buses in
    if not $ Set.null res then
      print $ Set.findMin res * (time - ts)
    else
      go (succ time)
  buses = Set.fromList $ map read $ filter (/= "x") buses'

part2 :: [String] -> IO ()
part2 buses' =
  let buses = filter ((/=) "x" . fst) $ List.zip buses' [0..] in
  --print $ List.sortOn fst buses
  let ls937 :: [Integer];
      ls937 = map (\x -> x - 44) $ map (937 * ) [1..] in
  let ls569 = filter (\x -> (x + 13) `mod` 569 == 0) ls937 in
  -- [("13",0),("17",61),("19",32),("23",36),("29",15),("37",50),("41",3),("569",13),("937",44)]
  let ls41 = filter (\x -> (x + 3) `mod` 41 == 0) ls569 in
  let ls37 = filter (\x -> (x + 50) `mod` 37 == 0) ls41 in
  let ls29 = filter (\x -> (x + 15) `mod` 29 == 0) ls37 in
  let ls23 = filter (\x -> (x + 36) `mod` 23 == 0) ls29 in
  let ls19 = filter (\x -> (x + 32) `mod` 19 == 0) ls23 in
  let ls17 = filter (\x -> (x + 61) `mod` 17 == 0) ls19 in
  let ls13 = filter (\x -> x `mod` 13 == 0) ls17 in

  print $ head ls13

solve :: String -> IO ()
solve = go . list id where
  go [ts', buses'] = do
    let ts = read ts'
    let buses = List.splitOn "," buses'
    part1 (ts, buses)
    part2 buses
