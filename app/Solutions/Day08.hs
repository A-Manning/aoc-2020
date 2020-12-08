module Solutions.Day08 where

import qualified List
import qualified Set
import qualified Vector

import Either (getLeft, maybeRight)
import Input hiding (get)
import Foldable (unsafeFindJust)
import Vector ((!))
import Function

eval :: Vector.T (String, Int) -> Either Int Int
eval instrs = go 0 0 Set.empty where
  go acc pos past =
    if pos >= Vector.length instrs then Right acc else
    if Set.member pos past then Left acc else
    case instrs ! pos of
      ("nop", _) -> go acc (succ pos) (Set.insert pos past)
      ("jmp", offset) -> go acc (pos + offset) (Set.insert pos past)
      ("acc", incr) -> go (acc + incr) (succ pos) (Set.insert pos past)

part1 :: Vector.T (String, Int) -> IO ()
part1 = print . getLeft . eval

part2 :: Vector.T (String, Int) -> IO ()
part2 instrs =
  Vector.findIndices notAcc instrs
    & Vector.map (\idx -> eval $ Vector.mapAt idx flipInstr instrs)
    & unsafeFindJust maybeRight
    & print
  where
    notAcc (instr, _) = instr /= "acc"
    flipInstr ("nop", arg) = ("jmp", arg)
    flipInstr ("jmp", arg) = ("nop", arg)

solve :: String -> IO ()
solve = go . vector (second parse . List.asPair . List.splitOn " ") where
  go pairs = do
    part1 pairs
    part2 pairs
  parse ('+':rest) = read rest
  parse ('-':rest) = -(read rest)
