{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day17 where

import qualified Data.Maybe as Maybe
import qualified List
import qualified Map
import qualified Matrix
import qualified Set
import qualified Tensor3
import qualified Tensor4
import qualified Vector

import Control.Monad (join)
import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)
import Matrix (Matrix(Matrix))
import Tensor3 (Tensor3(Tensor3))
import Tensor4 (Tensor4(Tensor4))

pad3 :: Tensor3.T a -> a -> Tensor3 a
pad3 (Tensor3 arr3) x =
  let (_, ylen, zlen) = Tensor3.shape (Tensor3 arr3) in
  let yrow = Vector.replicate (zlen + 2) x in
  let xrow = Vector.replicate (ylen + 2) yrow in
  arr3
  & fmap (fmap (Vector.cons x . flip Vector.snoc x))
  & fmap (Vector.cons yrow . flip Vector.snoc yrow)
  & (Vector.cons xrow . flip Vector.snoc xrow)
  & Tensor3

sliceX3 :: Int -> Int -> Tensor3 a -> Tensor3 a
sliceX3 i len = Tensor3 . Vector.safeSlice i len . Tensor3.toVectors

sliceY3 :: Int -> Int -> Tensor3 a -> Tensor3 a
sliceY3 i len = Tensor3 . fmap (Vector.safeSlice i len) . Tensor3.toVectors

sliceZ3 :: Int -> Int -> Tensor3 a -> Tensor3 a
sliceZ3 i len = Tensor3.toVectors
                >>> fmap (fmap (Vector.safeSlice i len))
                >>> Tensor3

shrinkBy3 :: (a -> Bool) -> Tensor3 a -> Tensor3 a
shrinkBy3 f arr3 =
  let (xlen, ylen, zlen) = Tensor3.shape arr3 in
  let (minx, miny, minz) = Maybe.fromMaybe (0, 0, 0) $ Tensor3.minima f arr3 in
  let (maxx, maxy, maxz) = Maybe.fromMaybe (pred xlen, pred ylen, pred zlen)
                           $ Tensor3.maxima f arr3 in
  sliceX3 minx (succ maxx - minx) arr3
  & sliceY3 miny (succ maxy - miny)
  & sliceZ3 minz (succ maxz - minz)

step3 :: Tensor3 Bool -> Tensor3 Bool
step3 arr3 =
  imap (\pos x ->
          case x of
            True -> activeNeighbors pos == 2 || activeNeighbors pos == 3
            False -> activeNeighbors pos == 3)
        space
  & shrinkBy3 id
  where
    space = pad3 arr3 False
    activeNeighbors (x, y, z) =
      (,,) <$> [pred x, x, succ x]
           <*> [pred y, y, succ y]
           <*> [pred z, z, succ z]
      & List.delete (x, y, z)
      & Maybe.mapMaybe (space `Tensor3.atMaybe`)
      & filter id
      & length

part1 :: Matrix Bool -> IO ()
part1 (Matrix arr2) = do
  print $ go 6 arr3
  where
    arr3 = Tensor3 $ fmap (fmap Vector.singleton) arr2
    go 0 space = do
      Tensor3.countBy id space
    go n space = do
      let next = step3 space
      go (pred n) next

pad4 :: Tensor4 a -> a -> Tensor4 a
pad4 (Tensor4 arr4) x =
  let (_, xlen, ylen, zlen) = Tensor4.shape (Tensor4 arr4) in
  let yrow = Vector.replicate (zlen + 2) x in
  let xrow = Vector.replicate (ylen + 2) yrow in
  let wrow = Vector.replicate (xlen + 2) xrow in
  arr4
  & fmap (fmap (fmap (Vector.cons x . flip Vector.snoc x)))
  & fmap (fmap (Vector.cons yrow . flip Vector.snoc yrow))
  & fmap (Vector.cons xrow . flip Vector.snoc xrow)
  & (Vector.cons wrow . flip Vector.snoc wrow)
  & Tensor4

sliceW4 :: Int -> Int -> Tensor4 a -> Tensor4 a
sliceW4 i len = Tensor4 . Vector.safeSlice i len . Tensor4.toVectors

sliceX4 :: Int -> Int -> Tensor4 a -> Tensor4 a
sliceX4 i len = Tensor4.toVectors
                >>> fmap (Tensor3.toVectors . sliceX3 i len . Tensor3)
                >>> Tensor4

sliceY4 :: Int -> Int -> Tensor4 a -> Tensor4 a
sliceY4 i len = Tensor4.toVectors
                >>> fmap (Tensor3.toVectors . sliceY3 i len . Tensor3)
                >>> Tensor4

sliceZ4 :: Int -> Int -> Tensor4 a -> Tensor4 a
sliceZ4 i len = Tensor4.toVectors
                >>> fmap (Tensor3.toVectors . sliceZ3 i len . Tensor3)
                >>> Tensor4

shrinkBy4 :: (a -> Bool) -> Tensor4 a -> Tensor4 a
shrinkBy4 f arr4 =
  let (wlen, xlen, ylen, zlen) = Tensor4.shape arr4 in
  let (minw, minx, miny, minz) = Maybe.fromMaybe (0, 0, 0, 0)
                                 $ Tensor4.minima f arr4 in
  let (maxw, maxx, maxy, maxz) = Tensor4.maxima f arr4
                                 & Maybe.fromMaybe (pred wlen,
                                                    pred xlen,
                                                    pred ylen,
                                                    pred zlen) in
  sliceW4 minw (succ maxw - minw) arr4
  & sliceX4 minx (succ maxx - minx)
  & sliceY4 miny (succ maxy - miny)
  & sliceZ4 minz (succ maxz - minz)

step4 :: Tensor4 Bool -> Tensor4 Bool
step4 arr4 =
  imap (\pos x ->
          case x of
            True -> activeNeighbors pos == 2 || activeNeighbors pos == 3
            False -> activeNeighbors pos == 3)
        space
  & shrinkBy4 id
  where
    space = pad4 arr4 False
    activeNeighbors (w, x, y, z) =
      (,,,) <$> [pred w, w, succ w]
            <*> [pred x, x, succ x]
            <*> [pred y, y, succ y]
            <*> [pred z, z, succ z]
      & List.delete (w, x, y, z)
      & Maybe.mapMaybe (space `Tensor4.atMaybe`)
      & filter id
      & length

part2 :: Matrix Bool -> IO ()
part2 (Matrix arr2) = do
  print $ go 6 arr4
  where
    arr4 = Tensor4 $ Vector.singleton $ fmap (fmap Vector.singleton) arr2
    go 0 space = Tensor4.countBy id space
    go n space = go (pred n) (step4 space)

solve :: String -> IO ()
solve = go . fmap parse . matrix where
  go m = do
    part1 m
    part2 m
  parse '.' = False
  parse '#' = True
