module Solutions where

import qualified Input
import qualified Solutions.Day01
import qualified Solutions.Day02
import qualified Solutions.Day03
import qualified Solutions.Day04
import qualified Solutions.Day05
import qualified Solutions.Day06
import qualified Solutions.Day07
import qualified Solutions.Day08
import qualified Solutions.Day09
import qualified Solutions.Day10
import qualified Solutions.Day11
import qualified Solutions.Day12
import qualified Solutions.Day13
import qualified Solutions.Day14
import qualified Solutions.Day15
import qualified Solutions.Day16
import qualified Solutions.Day17
import qualified Solutions.Day18
import qualified Solutions.Day19
import qualified Solutions.Day20
import qualified Solutions.Day21
import qualified Solutions.Day22
import qualified Solutions.Day23
import qualified Solutions.Day24
import qualified Solutions.Day25



solutions :: [String -> IO ()]
solutions = [ Solutions.Day01.solve
            , Solutions.Day02.solve
            , Solutions.Day03.solve
            , Solutions.Day04.solve
            , Solutions.Day05.solve
            , Solutions.Day06.solve
            , Solutions.Day07.solve
            , Solutions.Day08.solve
            , Solutions.Day09.solve
            , Solutions.Day10.solve
            , Solutions.Day11.solve
            , Solutions.Day12.solve
            , Solutions.Day13.solve
            , Solutions.Day14.solve
            , Solutions.Day15.solve
            , Solutions.Day16.solve
            , Solutions.Day17.solve
            , Solutions.Day18.solve
            , Solutions.Day19.solve
            , Solutions.Day20.solve
            , Solutions.Day21.solve
            , Solutions.Day22.solve
            , Solutions.Day23.solve
            , Solutions.Day24.solve
            , Solutions.Day25.solve ]

solutionsBB :: [String -> IO ()]
solutionsBB = [ Solutions.Day01.solveBB
              , Solutions.Day02.solveBB
              , Solutions.Day03.solveBB
              , Solutions.Day04.solveBB
              , Solutions.Day05.solveBB ]

solve :: Int -> IO ()
solve n = solutions !! pred n =<< Input.get n

solveBB :: Int -> IO ()
solveBB n = solutionsBB !! pred n =<< Input.getBB n
