module Solutions.Day04 where

import qualified Map
import qualified Set

import Map ((!))
import Function
import Foldable (allP, anyP, countBy)
import Input hiding (get)
import List ((!!), asPair, intercalate, splitOn)

hasRequiredFields :: Map.T String String -> Bool
hasRequiredFields = Set.isSubsetOf required . Map.keysSet where
  required = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

check :: Map.T String String -> Bool
check m = and [ hasRequiredFields m
              , allP [checkLen 4, digits, checkBound 1920 2002] (m ! "byr")
              , allP [checkLen 4, digits, checkBound 2010 2020] (m ! "iyr")
              , allP [checkLen 4, digits, checkBound 2020 2030] (m ! "eyr")
              , checkHgt (m ! "hgt")
              , checkHcl (m ! "hcl")
              , checkEcl (m ! "ecl")
              , allP [checkLen 9, digits] (m ! "pid") ] where
  bound lo hi = allP [(lo <=), (hi >=)]
  digits = all $ bound '0' '9'
  checkBound = read $$>> bound
  checkLen = length $>> (==)
  checkHgt = go . reverse where
    go ('m':'c':rest) = allP [digits, checkBound 150 193] (reverse rest)
    go ('n':'i':rest) = allP [digits, checkBound 59 76] (reverse rest)
    go _ = False
  checkHcl ('#':rest) =
    allP [checkLen 6, all (anyP [bound '0' '9', bound 'a' 'f'])] rest
  checkHcl _ = False
  checkEcl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

solve :: String -> IO ()
solve = go . map (parse . intercalate " ") . splitOn [""] . list id where
  parse = splitOn " " >>> map (splitOn ":" >>> asPair) >>> Map.fromList
  go passports = do
    print $ countBy hasRequiredFields passports
    print $ countBy check passports

solveBB :: String -> IO ()
solveBB = solve
