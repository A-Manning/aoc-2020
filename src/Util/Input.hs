module Util.Input where

import Text.Printf (printf)

-- get the nth problem input as a string
input :: Int -> IO String
input = readFile . printf "inputs/day%02d.txt"

-- get the nth problem input as lines
inputLines :: Int -> IO [String]
inputLines = (lines <$>) . input

-- read the nth problem lines
readInputLines :: Read a => Int -> IO [a]
readInputLines = (map read <$>) . inputLines
