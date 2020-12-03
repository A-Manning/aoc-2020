module Input where

import qualified Data.Vector as Vector
import qualified Matrix
import qualified Vector

import Text.Printf (printf)

import Function

-- get the nth problem input as a string
input :: Int -> IO String
input = readFile . printf "inputs/day%02d.txt"

-- get the nth problem input as a list of lines
inputList :: Int -> IO [String]
inputList = (lines <$>) . input

-- Apply the function to each line of the nth problem input, obtaining a list.
procInputList :: Int -> (String -> a) -> IO [a]
procInputList n f = map f <$> inputList n

-- read the nth problem input lines into a list
readInputList :: Read a => Int -> IO [a]
readInputList n = procInputList n read

{- Apply the function to each line of the nth problem input,
   obtaining a Vector. -}
procInputVector :: Int -> (String -> a) -> IO (Vector.T a)
procInputVector = procInputList >>$ fmap Vector.fromList

-- get the nth problem input as a vector of lines
inputVector :: Int -> IO (Vector.T String)
inputVector n = procInputVector n id

-- read the nth problem input lines into a vector
readInputVector :: Read a => Int -> IO (Vector.T a)
readInputVector n = procInputVector n read

-- read the nth problem input chars into a matrix
inputMatrix :: Int -> IO (Matrix.T Char)
inputMatrix n =
  Matrix.transpose . Matrix.Matrix <$> procInputVector n Vector.fromList
