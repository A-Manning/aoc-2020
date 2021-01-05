{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day16 where

import qualified List
import qualified Map
import qualified Set
import qualified Vector

import Foldable (anyP)
import Function
import Input hiding (get)
import Map ((!?))
import Vector ((!))

type Rule = (String, (Int, Int), (Int, Int))
type Ticket = Vector.T Int

bound :: (Int, Int) -> Int -> Bool
bound (lo, hi) x = lo <= x && x <= hi

satisfies :: Rule -> Int -> Bool
satisfies (_, range1, range2) x = bound range1 x || bound range2 x

isValidSomeRule :: [Rule] -> Int -> Bool
isValidSomeRule = anyP . List.map satisfies

part1 :: [Rule] -> [Ticket] -> IO ()
part1 rules tickets =
  map (sum . Vector.filter (not . isValidSomeRule rules)) tickets
  & sum
  & print

{-
part2 :: [Rule] -> [Ticket] -> IO ()
part2 rules tickets =
  let validTickets = filter (all $ isValidSomeRule rules) tickets in
  print $ map (\(ps,i) -> (map snd ps, i)) $ filter (\(ps, _) -> length ps == 13) $ zip (map possibilities [0..19]) [0..]
  where
    validTicks = Vector.fromList $ filter (all $ isValidSomeRule rules) tickets
    isPossible pos r = all (satisfies r) $ fmap (`Vector.at` pos) validTicks
    possibilities pos =
      fmap snd $ Vector.filter (isPossible pos . fst) (zip rules [0..])
    {-
    solve possibilities results =
      case Vector.findIndex ((==) 1 . Vector.length) possibilities of
        Just i ->
          let rule = Vector.head (possibilities `Vector.at` i) in
          let newResults = Map.insert i rule results in
          let newPossibilities = Vector.updateAt i [] possibilities in
          solve newPossibilities newResults
        Nothing ->
          if all ((==) 0 . Vector.length) possibilities then results else
          undefined
    -}
-}

solve :: String -> IO ()
solve = go . parseInput . list id where
  go (rules, myticket, nearby) = do
    part1 rules nearby
    --part2 rules nearby
    print $ product [ myticket ! 8
                    , myticket ! 4
                    , myticket ! 18
                    , myticket ! 9
                    , myticket ! 17
                    , myticket ! 7 ]
  parseInput lines =
    let [rules, myticket, nearby] = List.splitOn [""] lines in
    ( map parseRule rules
    , parseTicket $ head $ tail myticket
    , map parseTicket $ tail nearby )
  parseRule :: String -> Rule
  parseRule s =
    let [name, rest] = List.splitOn ": " s in
    let [range1, range2] = List.splitOn " or " rest in
    (name, parseRange range1, parseRange range2)
  parseRange s =
    let [lo, hi] = List.splitOn "-" s in
    (read lo, read hi)
  parseTicket :: String -> Ticket
  parseTicket = Vector.fromList . map read . List.splitOn ","
