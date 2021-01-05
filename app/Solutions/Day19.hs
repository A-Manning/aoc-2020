{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day19 where

import qualified Data.Maybe as Maybe
import qualified Either
import qualified List
import qualified Map
import qualified Set
import qualified Vector

import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)
import Map ((!))

data Rule = Char Char | All [Int] | Any [Rule]
  deriving Show

type Rules = Map.T Int Rule

parseLines :: [String] -> (Rules, [String])
parseLines lines =
  (Map.fromList $ map parseRule rules', testcases) where
    [rules', testcases] = List.splitOn [""] lines
    parseRule r =
      let [n, rest] = List.splitOn ": " r in
      case List.splitOn " | " rest of
        [left, right] -> (read n, Any [parseRule' left, parseRule' right])
        _ -> (read n, parseRule' rest)
    parseRule' r =
      case List.splitOn " " r of
        [['"', c, '"']] -> Char c
        rs -> All $ map read rs

checkRule :: String -> Int -> Rules -> Bool
checkRule s r rs =
  any (\case Just "" -> True; _ -> False) $ checkRule' s (rs ! r)
  where
    checkRule' (s0:sn) (Char c) = [if s0 == c then Just sn else Nothing]
    checkRule' s (All rules) =
      foldl (\ss r -> ss >>= (\case Nothing -> [Nothing]
                                    Just s1 -> checkRule' s1 (rs ! r)))
            [Just s]
            rules
    checkRule' s (Any rules) = rules >>= checkRule' s
    checkRule' _ _ = [Nothing]

part1 :: Rules -> [String] -> IO ()
part1 rules = print . length . filter (\tc -> checkRule tc 0 rules)

part2 :: Rules -> [String] -> IO ()
part2 = part1 . Map.insert 11 (Any [All [42, 31], All [42, 11, 31]])
              . Map.insert 8 (Any [All [42], All [42, 8]])

solve :: String -> IO ()
solve = go . parseLines . list id where
  go (rules, testcases) = do
    part1 rules testcases
    part2 rules testcases
