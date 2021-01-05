{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tensor4 where

import qualified Data.Maybe as Maybe
import qualified Foldable
import qualified Tensor3
import qualified Vector

import Function
import Functor (FunctorWithIndex(..))
import Tensor3 (Tensor3(Tensor3))

newtype Tensor4 a =
  Tensor4 {toVectors :: Vector.T (Vector.T (Vector.T (Vector.T a)))}
  deriving Eq

type T = Tensor4

instance Functor Tensor4 where
  fmap f = Tensor4 . fmap (Tensor3.toVectors . fmap f . Tensor3) . toVectors

instance FunctorWithIndex (Int, Int, Int, Int) Tensor4 where
  imap f = toVectors
            >>> imap (\w -> Tensor3
                            >>> imap (\(x, y, z) -> f (w, x, y, z))
                            >>> Tensor3.toVectors)
            >>> Tensor4

-- safe element access
atMaybe :: Tensor4 a -> (Int, Int, Int, Int) -> Maybe a
atMaybe (Tensor4 arr4) (w, x, y, z) =
  arr4 `Vector.atMaybe` w
  >>= (Tensor3 >>> (`Tensor3.atMaybe` (x, y, z)))

at :: Tensor4 a -> (Int, Int, Int, Int) -> a
at t pos = Maybe.fromJust (t `atMaybe` pos)

shape :: Tensor4 a -> (Int, Int, Int, Int)
shape (Tensor4 arr4) =
  let (x, y, z) = arr4 `Vector.atMaybe` 0
                  & fmap (Tensor3 >>> Tensor3.shape)
                  & Maybe.fromMaybe (0, 0, 0) in
  (Vector.length arr4, x, y, z)

countBy :: (a -> Bool) -> Tensor4 a -> Int
countBy f = sum . fmap (Tensor3.countBy f . Tensor3) . toVectors

minima :: (a -> Bool) -> Tensor4 a -> Maybe (Int, Int, Int, Int)
minima f (Tensor4 arr4) = do
  let maybeMinima = fmap (Tensor3.minima f . Tensor3) arr4
  minw <- Foldable.findIndex Maybe.isJust maybeMinima
  let minima' = Vector.catMaybes maybeMinima
  let minx = minimum $ fmap (\(x, _, _) -> x) minima'
  let miny = minimum $ fmap (\(_, y, _) -> y) minima'
  let minz = minimum $ fmap (\(_, _, z) -> z) minima'
  return (minw, minx, miny, minz)

maxima :: (a -> Bool) -> Tensor4 a -> Maybe (Int, Int, Int, Int)
maxima f (Tensor4 arr4) = do
  let wlen = Vector.length arr4
  let maybeMaxima = fmap (Tensor3.maxima f . Tensor3) arr4
  maxw <- fmap (pred wlen -) $ Foldable.findIndex Maybe.isJust
                             $ Vector.reverse maybeMaxima
  let maxima' = Vector.catMaybes maybeMaxima
  let maxx = minimum $ fmap (\(x, _, _) -> x) maxima'
  let maxy = minimum $ fmap (\(_, y, _) -> y) maxima'
  let maxz = minimum $ fmap (\(_, _, z) -> z) maxima'
  return (maxw, maxx, maxy, maxz)
