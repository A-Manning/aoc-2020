module Input where

import qualified Data.Vector as Vector
import qualified Matrix
import qualified Vector

import Text.Printf (printf)

import Function

get :: Int -> IO String
get = readFile . printf "inputs/day%02d.txt"

getBB :: Int -> IO String
getBB = readFile . printf "bigboys/day%02d.txt"

-- read the nth problem input lines into a list
list :: (String -> a) -> String -> [a]
list f = fmap f . lines

-- read the nth problem input lines into a vector
vector :: (String -> a) -> String -> Vector.T a
vector = Vector.fromList $<< list

-- read the nth problem input chars into a matrix
matrix :: String -> Matrix.T Char
matrix = Matrix.transpose . Matrix.Matrix . vector Vector.fromList
