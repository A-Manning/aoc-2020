{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day22 where

import qualified Data.Maybe as Maybe
import qualified Either
import qualified List
import qualified Map
import qualified Matrix
import qualified Set
import qualified Vector

import Data.Hashable (hash)
import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)

playGame1 :: ([Int], [Int]) -> [Int]
playGame1 (x:xs, y:ys) =
  case if x > y then (xs ++ [x, y], ys) else (xs, ys ++ [y, x]) of
    ([], p2') -> p2'
    (p1', []) -> p1'
    (p1', p2') -> playGame1 (p1', p2')

score :: [Int] -> Int
score deck = sum $map (\(c, s) -> c * s) $zip (reverse deck) [1..]

part1 :: ([Int], [Int]) -> IO ()
part1 = print . score . playGame1

playGame2 :: Set.T Int -> ([Int], [Int]) -> Either [Int] [Int]
playGame2 hs (x:xs, y:ys) =
  let h = hash (x:xs, y:ys) in
  if Set.member h hs then Left (x:xs) else
  let hs' = Set.insert h hs in
  let next = if length xs >= x && length ys >= y then
                case playGame2 Set.empty (take x xs, take y ys) of
                  Left _ -> (xs ++ [x, y], ys)
                  Right _ -> (xs, ys ++ [y, x])
             else
              if x > y then (xs ++ [x, y], ys) else (xs, ys ++ [y, x]) in
  case next of
    ([], p2') -> Right p2'
    (p1', []) -> Left p1'
    (p1', p2') -> playGame2 hs' (p1', p2')

part2 :: ([Int], [Int]) -> IO ()
part2 = print . score . Either.fromEither . playGame2 Set.empty

solve :: String -> IO ()
solve = go . parseDecks . list id where
  go decks = do part1 decks; part2 decks
  parseDecks lines =
    let [p1, p2] = List.splitOn [""] lines in
    (map read $tail p1, map read $tail p2)
