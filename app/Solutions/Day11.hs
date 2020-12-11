module Solutions.Day11 where

import qualified Matrix

import Foldable (countBy)
import Input hiding (get)
import Matrix ((!?))

tick1 :: Matrix.T Char -> Matrix.T Char
tick1 m = Matrix.imap go m where
  go pos 'L' = if occupiedAdjacent pos == 0 then '#' else 'L'
  go pos '#' = if occupiedAdjacent pos >= 4 then 'L' else '#'
  go   _ '.' = '.'
  occupiedAdjacent (x, y) = countBy (== Just '#') $ map (m !?) $
    [ (pred x, succ y), (x, succ y), (succ x, succ y)
    , (pred x,      y),              (succ x,      y)
    , (pred x, pred y), (x, pred y), (succ x, pred y) ]

tick2 :: Matrix.T Char -> Matrix.T Char
tick2 m = Matrix.imap go m where
  go pos 'L' = if occupiedAdjacent pos == 0 then '#' else 'L'
  go pos '#' = if occupiedAdjacent pos >= 5 then 'L' else '#'
  go   _ '.' = '.'
  occupiedBeam (x, y) (down, right) = case m !? (x + right, y + down) of
      Nothing  -> False
      Just 'L' -> False
      Just '#' -> True
      Just '.' -> occupiedBeam (x + right, y + down) (down, right)
  occupiedAdjacent pos = countBy id $ map (occupiedBeam pos) $
    [ (-1, -1), (-1, 0), (-1, 1)
    , ( 0, -1),          ( 0, 1)
    , ( 1, -1), ( 1, 0), ( 1, 1) ]

solve :: String -> IO ()
solve = go . matrix where
  go m = do
    print $ occupied $ simulate tick1 m
    print $ occupied $ simulate tick2 m
  occupied = sum . fmap (countBy (== '#')) . Matrix.toVector
  simulate f m = let next = f m in if next == m then m else simulate f next
