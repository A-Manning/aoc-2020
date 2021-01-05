{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tensor3 where

import qualified Data.Maybe as Maybe
import qualified Foldable
import qualified Matrix
import qualified Vector

import Function
import Functor (FunctorWithIndex(..))
import Matrix (Matrix(Matrix))

newtype Tensor3 a = Tensor3 {toVectors :: Vector.T (Vector.T (Vector.T a))}
  deriving Eq

type T = Tensor3

instance Functor Tensor3 where
  fmap f = Tensor3 . fmap (Matrix.toVectors . fmap f . Matrix) . toVectors

instance FunctorWithIndex (Int, Int, Int) Tensor3 where
  imap f = toVectors
            >>> imap (\x -> Matrix
                            >>> imap (\(y, z) -> f (x, y, z))
                            >>> Matrix.toVectors)
            >>> Tensor3

-- safe element access
atMaybe :: Tensor3 a -> (Int, Int, Int) -> Maybe a
atMaybe (Tensor3 arr3) (x, y, z) =
  arr3 `Vector.atMaybe` x
  >>= (Matrix >>> (`Matrix.atMaybe` (y, z)))

at :: Tensor3 a -> (Int, Int, Int) -> a
at t pos = Maybe.fromJust (t `atMaybe` pos)

shape :: Tensor3 a -> (Int, Int, Int)
shape (Tensor3 arr3) =
  let (y, z) = arr3 `Vector.atMaybe` 0
               & fmap (Matrix >>> Matrix.shape)
               & Maybe.fromMaybe (0, 0) in
  (Vector.length arr3, y, z)

countBy :: (a -> Bool) -> Tensor3 a -> Int
countBy f = sum . fmap (Matrix.countBy f . Matrix) . toVectors

minima :: (a -> Bool) -> Tensor3 a -> Maybe (Int, Int, Int)
minima f (Tensor3 arr3) = do
  let maybeMinima = fmap (Matrix.minima f . Matrix) arr3
  minx <- Foldable.findIndex Maybe.isJust maybeMinima
  let minima' = Vector.catMaybes maybeMinima
  let miny = minimum $ fmap fst $ minima'
  let minz = minimum $ fmap snd $ minima'
  return (minx, miny, minz)

maxima :: (a -> Bool) -> Tensor3 a -> Maybe (Int, Int, Int)
maxima f (Tensor3 arr3) = do
  let xlen = Vector.length arr3
  let maybeMaxima = fmap (Matrix.maxima f . Matrix) arr3
  maxx <- fmap (pred xlen -) $ Foldable.findIndex Maybe.isJust
                             $ Vector.reverse maybeMaxima
  let maxima' = Vector.catMaybes maybeMaxima
  let maxy = maximum $ fmap fst $ maxima'
  let maxz = maximum $ fmap snd $ maxima'
  return (maxx, maxy, maxy)
