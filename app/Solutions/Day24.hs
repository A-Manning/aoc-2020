{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day24 where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Either
import qualified Foldable
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
import Matrix (Matrix(..))

data Direction = E | W | NE | NW | SE | SW deriving (Eq, Show)

-- ODD ROWS ARE OFFSET TO THE RIGHT
type Floor = Matrix Bool

midpoint :: (Int, Int)
midpoint = (150, 150)

initfloor :: Floor
initfloor = Matrix.generate 300 300 (\_ _ -> True)

parseDirections :: String -> [Direction]
parseDirections [] = []
parseDirections ('e':rest) = E:parseDirections rest
parseDirections ('w':rest) = W:parseDirections rest
parseDirections ('n':'e':rest) = NE:parseDirections rest
parseDirections ('n':'w':rest) = NW:parseDirections rest
parseDirections ('s':'e':rest) = SE:parseDirections rest
parseDirections ('s':'w':rest) = SW:parseDirections rest

nextpos :: (Int, Int) -> Direction -> (Int, Int)
nextpos (x, y) E = (succ x, y)
nextpos (x, y) W = (pred x, y)
nextpos (x, y) NE = (if y `mod` 2 == 0 then x else succ x, succ y)
nextpos (x, y) NW = (if y `mod` 2 == 0 then pred x else x, succ y)
nextpos (x, y) SE = (if y `mod` 2 == 0 then x else succ x, pred y)
nextpos (x, y) SW = (if y `mod` 2 == 0 then pred x else x, pred y)

fliptiles :: Floor -> [Direction] -> Floor
fliptiles = go midpoint where
  go pos floor [] = Matrix.mapAt pos not floor
  go pos floor (d:ds) =
    let newpos = nextpos pos d in
    --let newfloor = Matrix.mapAt newpos not floor in
    go newpos floor ds

part1 :: _ -> IO ()
part1 = print . Matrix.countBy not . foldl fliptiles initfloor

blackNeighbors :: (Int, Int) -> Floor -> Int
blackNeighbors pos floor =
  Foldable.countBy (==Just False) $map ((floor `Matrix.atMaybe`) . nextpos pos)
                                  $[E, W, NE, NW, SE, SW]

step :: Floor -> Floor
step floor = imap (\pos v ->
                   let bn = blackNeighbors pos floor in
                   case v of
                     True -> not (bn == 2)
                     False -> bn == 0 || bn > 2)
                  floor

part2 :: _ -> IO ()
part2 paths = print $Matrix.countBy not $go 100 initial
  where
    initial = foldl fliptiles initfloor paths
    go 0 floor = floor
    go n floor = go (pred n) $step floor

solve :: String -> IO ()
solve = go . list parseDirections where
  go dirs = do
    part1 dirs
    part2 dirs
