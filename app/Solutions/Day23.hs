{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day23 where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Either
import qualified List
import qualified Map
import qualified Matrix
import qualified Set
import qualified Vector

import Data.Hashable (hash)
import Data.Sequence (Seq(..), (><))
import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)

step :: Seq Int -> Seq Int
step (cur:<|rest0) =
  preDest >< pickup >< postDest >< Seq.singleton cur
  where
    (pickup, rest1) = Seq.splitAt 3 rest0
    minCup = minimum rest1
    maxCup = maximum rest1
    (preDest, postDest) = splitDest (pred cur)
    splitDest t =
      case findIndex (==t) rest1 of
        Just i -> Seq.splitAt (succ i) rest1
        Nothing ->
          if pred t < minCup then splitDest maxCup else splitDest (pred t)

playGame :: Int -> Seq Int -> Seq Int
playGame 0 !cups = cups
playGame !n !cups = playGame (pred n) $ step cups

part1 :: _ -> IO ()
part1 cups =
  putStrLn $mconcat $toList $fmap show $Seq.drop 1 (pre >< post)
  where
    res = playGame 100 cups
    idx = unsafeFindIndex (==1) res
    (post, pre) = Seq.splitAt idx res

playGame1 :: Int -> Seq Int -> IO (Seq Int)
playGame1 0 !cups = return cups
playGame1 !n !cups = do
  if n `mod` 10 == 0 then
    print n
  else
    return ()
  playGame1 (pred n) $ step cups

part2 :: _ -> IO ()
part2 cups0 = do
  let cups1 = cups0 >< Seq.fromList [succ (maximum cups0) .. 1_000_000]
  res <- playGame1 10_000_000 cups1
  let idx = unsafeFindIndex (==1) res
  let (post, pre) = Seq.splitAt idx res
  print $Seq.drop 1 $Seq.take 3 (pre >< post)

solve :: String -> IO ()
solve = go . Seq.fromList . map Char.digitToInt . head . list id where
  go cups = do
    part1 cups
    part2 cups
