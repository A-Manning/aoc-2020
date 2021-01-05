{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day20 where

import qualified Data.Maybe as Maybe
import qualified Either
import qualified List
import qualified Map
import qualified Matrix
import qualified Set
import qualified Vector

import Data.Array
import Data.Maybe
import Data.List
import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)
import Matrix (Matrix(..))

split _ [] = [[]]
split d (x:xs) | d == x = [] : split d xs
               | otherwise = let (xs':xss) = split d xs in (x:xs') : xss

type Tile = (Int, Array (Int, Int) Char)

readTile :: [String] -> Tile
readTile xs = (id, readGrid $ tail xs)
  where id = read $ take 4 $ drop 5 $ head xs

readGrid ls = listArray ((0, 0), (h-1, w-1)) (concat ls)
  where w = length $ head ls
        h = length ls

rotate (id, g) = (id, listArray (bounds g) [g!(h-j, i) | (i, j) <- indices g])
  where (_,(h, _)) = bounds g

flipH (id, g) = (id, listArray (bounds g) [g!(i, w-j) | (i, j) <- indices g])
  where (_,(_, w)) = bounds g

orientations tile = let tiles = take 4 (iterate rotate tile) in tiles ++ map flipH tiles

findLayout tiles = search [head tiles] [head tiles] (head tiles : (concatMap orientations $ tail tiles))
  where search [] _ _ = []
        search (t:ts) seen candidates = (fst t, matches):search (ts++new) (seen++new) candidates'
          where matches = map (\f -> find (f t) (seen ++candidates)) [matchNorth, matchEast, matchSouth, matchWest]
                new = (catMaybes matches) `diff` seen
                candidates' = candidates `diff` new
                diff a b = filter (not . (`elem` (map fst b)) . fst) a

matchNorth (_, g1) (_, g2) = and [g1 ! (0, i) == g2 ! (9, i) | i <- [0..9]]
matchEast (_, g1) (_, g2) = and [g1 ! (i, 9) == g2 ! (i, 0) | i <- [0..9]]
matchSouth (_, g1) (_, g2) = and [g1 ! (9, i) == g2 ! (0, i) | i <- [0..9]]
matchWest (_, g1) (_, g2) = and [g1 ! (i, 0) == g2 ! (i, 9) | i <- [0..9]]

buildImage tiles layout = array ((0,0), (size * 8 - 1, size * 8 - 1)) (buildRow layout 0 start)
  where topLeft = fst $ fromJust $ find (\(_, m) -> isNothing (m!!0) && isNothing (m!!3)) layout
        start = find ((==topLeft) . fst) tiles
        size = floor $ sqrt $ fromIntegral $ length tiles

buildRow _ _ Nothing = []
buildRow layout i t@(Just (id, _)) = buildTile layout (i, 0) t ++ buildRow layout (i+1) (neighbors!!2)
  where neighbors = fromJust (lookup id layout)

buildTile _ _ Nothing = []
buildTile layout (i, j) (Just (id, g)) = tile ++ buildTile layout (i, j+1) (neighbors!!1)
  where neighbors = fromJust (lookup id layout)
        tile = [((i*8+k, j*8+l), g!(k+1, l+1)) | k <- [0..7], l <- [0..7]]

findMonsters monster (_, image) = [(i, j) | i <-[0..h-h'], j<-[0..w-w'],hasMonster (i, j)]
  where hasMonster (i, j) = and [image!(i+k,j+l)=='#'|(k,l)<-pattern]
        pattern=map fst $filter ((=='#').snd) $ assocs monster
        (_,(h, w))=bounds image
        (_,(h',w'))=bounds monster

partone layout = product $ map fst $ filter ((==2) . length . catMaybes . snd) layout

parttwo tiles layout monster = (length $ filter (=='#') $ elems image) - monsters * (length $filter (=='#') $ elems monster)
  where image = buildImage tiles layout
        monsters = length $ fromJust $ find (not . null) $ map (findMonsters monster) $ orientations (0, image)

solve :: String -> IO ()
solve input = do
  monster <- readFile "inputs/monster.txt"
  let tiles = map readTile $ split "" $ lines input
      layout = findLayout tiles
      answer1 = partone layout
      answer2 = parttwo tiles layout (readGrid $ lines monster)
  print answer1
  print answer2
