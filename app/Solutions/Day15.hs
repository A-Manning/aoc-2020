{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day15 where

import qualified List
import qualified Map
import qualified Set
import qualified Vector

import Input hiding (get)
import Map ((!?))

step :: (Int, Map.T Int Int, Int) -> (Int, Map.T Int Int, Int)
step (!lastTurn, !lastSeen, !last) =
  let ts = case lastSeen !? last of Nothing -> lastTurn; Just ts -> ts in
  (succ lastTurn, Map.insert last lastTurn lastSeen, lastTurn - ts)

evalTo :: Int -> (Int, Map.T Int Int, Int) -> Int
evalTo n (lastTurn, lastSeen, last) =
  if lastTurn < n then evalTo n $ step (lastTurn, lastSeen, last) else last

solve :: String -> IO ()
solve = go . initialize . map read . List.splitOn "," where
  go initial = do
    print $ evalTo 2020 initial
    print $ evalTo 30000000 initial
  initialize (x:xs) = foldl (\(t, m, v) v' -> (succ t, Map.insert v t m, v'))
                        (1, Map.empty, x)
                        xs
