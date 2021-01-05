{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Matrix where

import qualified Data.Maybe as Maybe
import qualified Foldable
import qualified List
import qualified Vector

import Data.Functor.Identity(Identity(..))
import Function
import Functor (FunctorWithIndex(..))


{- A 2D matrix. The inner vectors are the columns.
   A matrix with zero height must have zero width, and vice versa. -}
newtype Matrix a = Matrix {toVectors :: Vector.T (Vector.T a)}
  deriving (Eq)

type T a = Matrix a

-- properties
width :: Matrix a -> Int
width = Vector.length . toVectors

height :: Matrix a -> Int
height = toVectors
         >>> (`Vector.atMaybe` 0)
         >>> fmap Vector.length
         >>> Maybe.fromMaybe 0

shape :: Matrix a -> (Int, Int)
shape m = (width m, height m)

--
-- slice access
--

-- row access
(-!) :: Matrix a -> Int -> Vector.T a
(-!) v j = (`Vector.at` j) <$> toVectors v

row :: Matrix a -> Int -> Vector.T a
row = (-!)

-- safe row access
(-!?) :: Matrix a -> Int -> Maybe (Vector.T a)
(-!?) v j = Vector.sequence $ (`Vector.atMaybe` j) <$> toVectors v

rowMaybe :: Matrix a -> Int -> Maybe (Vector.T a)
rowMaybe = (-!?)

-- wrapping row access
(-!%) :: Matrix a -> Int -> Vector.T a
(-!%) v j = v -! (j `mod` height v)

rowWrap :: Matrix a -> Int -> Vector.T a
rowWrap = (-!%)

-- column access
(|!) :: Matrix a -> Int -> Vector.T a
(|!) = Vector.at . toVectors

col :: Matrix a -> Int -> Vector.T a
col = (|!)

-- safe column access
(|!?) :: Matrix a -> Int -> Maybe (Vector.T a)
(|!?) = Vector.atMaybe . toVectors

colMaybe :: Matrix a -> Int -> Maybe (Vector.T a)
colMaybe = (|!?)

-- wrapping column access
(|!%) :: Matrix a -> Int -> Vector.T a
(|!%) v j = v |! (j `mod` width v)

colWrap :: Matrix a -> Int -> Vector.T a
colWrap = (|!%)

--
-- element access
--

-- element access
(!) :: Matrix a -> (Int, Int) -> a
(!) v (x, y) = v |! x `Vector.at` y

at :: Matrix a -> (Int, Int) -> a
at = (!)

-- safe element access
(!?) :: Matrix a -> (Int, Int) -> Maybe a
(!?) v (x, y) = v |!? x >>= (`Vector.atMaybe` y)

atMaybe :: Matrix a -> (Int, Int) -> Maybe a
atMaybe = (!?)

-- row-wrapping element access
(!%-) :: Matrix a -> (Int, Int) -> a
(!%-) v (x, y) = v ! (x, y `mod` height v)

atWrapRow :: Matrix a -> (Int, Int) -> a
atWrapRow = (!%-)

-- row-wrapping safe element access
(!?%-) :: Matrix a -> (Int, Int) -> Maybe a
(!?%-) v (x, y) = v !? (x, y `mod` height v)

atMaybeWrapRow :: Matrix a -> (Int, Int) -> Maybe a
atMaybeWrapRow = (!?%-)

-- column-wrapping element access
(!%|) :: Matrix a -> (Int, Int) -> a
(!%|) v (x, y) = v ! (x `mod` width v, y)

atWrapCol :: Matrix a -> (Int, Int) -> a
atWrapCol = (!%|)

-- column-wrapping safe element access
(!?%|) :: Matrix a -> (Int, Int) -> Maybe a
(!?%|) v (x, y) = v !? (x `mod` width v, y)

atMaybeWrapCol :: Matrix a -> (Int, Int) -> Maybe a
atMaybeWrapCol = (!?%|)

-- wrapping element access
(!%) :: Matrix a -> (Int, Int) -> a
(!%) v (x, y) = v ! (x `mod` width v, y `mod` height v)

atWrap :: Matrix a -> (Int, Int) -> a
atWrap = (!%)

--
-- Construction
--

-- initialise from width, height, and a monadic function on the positions.
generateM :: Monad m => Int -> Int -> (Int -> Int -> m a) -> m (Matrix a)
generateM 0 0 _ = return $ Matrix Vector.empty
generateM 0 _ _ = undefined
generateM _ 0 _ = undefined
generateM w h f = Matrix <$> Vector.generateM w (Vector.generateM h . f)

-- initialise from width, height, and a function on the positions.
generate :: Int -> Int -> (Int -> Int -> a) -> Matrix a
generate = (>>$ Identity) $$>> generateM >>$$ runIdentity

--
-- Unary operations
--

transpose :: Matrix a -> Matrix a
transpose m = generate (height m) (width m) (\x y -> m ! (y, x))

rotate :: Matrix a -> Matrix a
rotate m =
  let (w, h) = shape m in
  Vector.fromList [0..pred h]
  & fmap (\c -> row m (pred h-c))
  & Matrix

-- flip across |
flipH :: Matrix a -> Matrix a
flipH = Matrix . Vector.reverse . toVectors

instance Show a => Show (Matrix a) where
  show = List.intercalate "\n" . Vector.toList
         . fmap show . toVectors . transpose

---
--- Mapping
---

instance FunctorWithIndex (Int, Int) Matrix where
  imap f = toVectors >>> imap (\x -> imap $ \y -> f (x, y)) >>> Matrix

instance Functor Matrix where
  fmap f = Matrix . fmap (fmap f) . toVectors

---
--- Folding
---

countBy :: (a -> Bool) -> Matrix a -> Int
countBy f = sum . fmap (Foldable.countBy f) . toVectors

minima :: (a -> Bool) -> Matrix a -> Maybe (Int, Int)
minima f m = do
  let yindices = fmap (Foldable.findIndex f) $ toVectors m
  minx <- Foldable.findIndex Maybe.isJust yindices
  let miny = minimum $ Vector.catMaybes yindices
  return (minx, miny)

maxima :: (a -> Bool) -> Matrix a -> Maybe (Int, Int)
maxima f m = do
  let (xlen, ylen) = shape m
  let yindices = fmap (Vector.reverse
                       >>>Foldable.findIndex f
                       >>> fmap (pred ylen -))
                  $ toVectors m
  maxx <- Vector.reverse yindices
            & Foldable.findIndex Maybe.isJust
            & fmap (pred xlen -)
  let maxy = maximum $ Vector.catMaybes yindices
  return (maxx, maxy)

---
--- Updates
---

mapAt :: (Int, Int) -> (a -> a) -> Matrix a -> Matrix a
mapAt (x, y) f = Matrix . Vector.mapAt x (Vector.mapAt y f) . toVectors
