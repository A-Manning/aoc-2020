{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day14 where

import qualified List
import qualified Map
import qualified Set
import qualified Vector

import Function
import Input hiding (get)
import Util.Bits (fromListBE, toListBE)

data Instr = Mask [Maybe Bool] | Mem Int Int
  deriving Show

applyMask1 :: [Maybe Bool] -> Int -> Int
applyMask1 mask v =
  let v' = reverse $ take 36 $ reverse $ toListBE v in
  fromListBE $ zipWith (\b m -> case m of Nothing -> b; Just m' -> m')
    v' mask

part1 :: [Instr] -> IO ()
part1 instrs = do
  let (finMask, finMap) = foldl applyInstr (noMask, Map.empty) instrs
  print $ sum $ map snd $ Map.toList finMap
  where
    noMask = map (const Nothing) [1..36]
    applyInstr (_, fm) (Mask m) = (m, fm)
    applyInstr (m, fm) (Mem addr v) =
      (m, Map.insert addr (applyMask1 m v) fm)

applyMask2' :: [Either Bool Bool] -> Int -> Int
applyMask2' mask v =
  let v' = reverse $ take 36 $ reverse $ toListBE v in
  fromListBE $ zipWith (\b m -> case m of Left v -> v; Right True -> b)
    v' mask

applyMask2 :: Vector.T (Either Bool Bool) -> Int -> [Int]
applyMask2 mask v =
  let i = Vector.findIndex (== Right False) mask in
  case i of
    Nothing -> [applyMask2' (Vector.toList mask) v]
    Just i ->
      applyMask2 (Vector.updateAt i (Left True) mask) v
      ++ applyMask2 (Vector.updateAt i (Left False) mask) v

part2 :: [Instr] -> IO ()
part2 instrs = do
  let (finMask, finMap) = foldl applyInstr (noMask, Map.empty) instrs
  print $ sum $ map snd $ Map.toList finMap
  where
    noMask = map (const Nothing) [1..36]
    applyInstr (_, fm) (Mask m) = (m, fm)
    applyInstr (m, fm) (Mem addr v) =
      let m' = fmap (\case Just True -> Left True
                           Just False -> Right True
                           Nothing -> Right False)
               (Vector.fromList m) in
      let addrs = applyMask2 m' addr in
      let fm' = foldr (flip Map.insert v) fm addrs in
      (m, fm')

solve :: String -> IO ()
solve = go . list parse where
  go instrs = do
    part1 instrs
    part2 instrs
  parse s =
    let [lhs, rhs] = List.splitOn " = " s in
    case lhs of
      "mask" -> Mask $ parseMask rhs
      _ ->
        let loc = read $ reverse $ drop 1 $ reverse $ drop 4 $ lhs in
        Mem loc $ read rhs
  parseMask = map (\case 'X' -> Nothing; '0' -> Just False; '1' -> Just True)
