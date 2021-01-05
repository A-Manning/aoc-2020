{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day25 where

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

transform :: Int -> Int -> Int
transform subject loop = go 1 0 where
  go v ctr = if ctr == loop then v else
             go ((v * subject)`mod`20201227) (succ ctr)

-- get the counter
unTransform :: Int -> Int -> Int
unTransform subject tgt = go 1 0 where
  go v ctr = if v == tgt then ctr else
             go ((v * subject) `mod` 20201227) (succ ctr)

solve :: String -> IO ()
solve = go . list read where
  go [cardpk, doorpk] = print $transform doorpk $unTransform 7 cardpk
