module Solutions where

import qualified Input
import qualified Solutions.Day01
import qualified Solutions.Day02
import qualified Solutions.Day03
import qualified Solutions.Day04
import qualified Solutions.Day05
import qualified Solutions.Day06
import qualified Solutions.Day07


solutions :: [String -> IO ()]
solutions = [ Solutions.Day01.solve
            , Solutions.Day02.solve
            , Solutions.Day03.solve
            , Solutions.Day04.solve
            , Solutions.Day05.solve
            , Solutions.Day06.solve
            , Solutions.Day07.solve ]

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
