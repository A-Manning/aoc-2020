module Solutions.Day09 where

import qualified List
import qualified Vector

import Input hiding (get)

firstInvalid :: Int -> Vector.T Int -> Int
firstInvalid preambleLength nums = go 0 ((+) <$> slice 0 <*> slice 0) where
  slice i = Vector.slice i preambleLength nums
  go i sums =
    let candidate = nums `Vector.at` (i + preambleLength) in
    if candidate `Vector.notElem` sums then candidate else
    let newSums = fmap (candidate +) $ slice i in
    go (succ i) $ Vector.concat [Vector.drop preambleLength sums, newSums]

contiguousSum :: Int -> Vector.T Int -> Vector.T Int
contiguousSum target nums = go (nums `Vector.at` 0) 0 0 where
  go s i j =
    if s == target then Vector.slice i (succ (j-i)) nums else
    if s > target then
      let s' = s - (nums `Vector.at` i) - (nums `Vector.at` j) in
      go s' (succ i) (pred j)
    else
      let s' = s + (nums `Vector.at` succ j) in
      go s' i (succ j)

solve :: String -> IO ()
solve = go . vector (read . head . List.splitOn " ") where
  go nums = do
    let inv = firstInvalid 25 nums
    print inv
    let csum = contiguousSum inv nums
    print $ minimum csum + maximum csum
