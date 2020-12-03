module Matrix where

import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector
import qualified Vector

import Data.Functor.Identity(Identity(..))

import Function

{- A 2D matrix. The inner vectors are the columns.
   A matrix with zero height must have zero width, and vice versa. -}
newtype Matrix a = Matrix {toVector :: Vector.T (Vector.T a)}

type T a = Matrix a

-- properties
width :: Matrix a -> Int
width = Vector.length . toVector

height :: Matrix a -> Int
height = toVector
         >>> (`Vector.atMaybe` 0)
         >>> fmap Vector.length
         >>> Maybe.fromMaybe 0

--
-- slice access
--

-- row access
(-!) :: Matrix a -> Int -> Vector.T a
(-!) v j = (`Vector.at` j) <$> toVector v

row :: Matrix a -> Int -> Vector.T a
row = (-!)

-- safe row access
(-!?) :: Matrix a -> Int -> Maybe (Vector.T a)
(-!?) v j = Vector.sequence $ (`Vector.atMaybe` j) <$> toVector v

rowMaybe :: Matrix a -> Int -> Maybe (Vector.T a)
rowMaybe = (-!?)

-- wrapping row access
(-!%) :: Matrix a -> Int -> Vector.T a
(-!%) v j = v -! (j `mod` height v)

rowWrap :: Matrix a -> Int -> Vector.T a
rowWrap = (-!%)

-- column access
(|!) :: Matrix a -> Int -> Vector.T a
(|!) = Vector.at . toVector

col :: Matrix a -> Int -> Vector.T a
col = (|!)

-- safe column access
(|!?) :: Matrix a -> Int -> Maybe (Vector.T a)
(|!?) = Vector.atMaybe . toVector

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
