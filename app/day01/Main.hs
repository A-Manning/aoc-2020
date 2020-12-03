module Main where

import Data.Set (Set, fromList, member, toList)
import Foldable (firstJust)
import Function ((&))
import Input (readInputList)

ksum :: Int -> Int -> Set Int -> Maybe [Int]
ksum 0 _ _ = Nothing
ksum 1 n s = if n `member` s then Just [n] else Nothing
ksum k n s = toList s
             & map (\x -> (x:) <$> ksum (pred k) (n-x) s)
             & firstJust

ksum2020Product :: Int -> Set Int -> Maybe Int
ksum2020Product k = fmap product . ksum k 2020

main :: IO ()
main = do
  inputSet <- fromList <$> readInputList 01
  print $ ksum2020Product 2 inputSet
  print $ ksum2020Product 3 inputSet
