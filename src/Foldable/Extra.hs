{-# LANGUAGE LambdaCase #-}
module Foldable.Extra where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Either

import Function

allP :: Foldable f => f (a -> Bool) -> a -> Bool
allP ls x = all ($ x) ls

anyP :: Foldable f => f (a -> Bool) -> a -> Bool
anyP ls x = any ($ x) ls

countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy f = length . filter f . Foldable.toList

count :: (Eq a, Foldable f) => a -> f a -> Int
count = countBy . (==)

unsafeFind :: Foldable f => (a -> Bool) -> f a -> a
unsafeFind = Foldable.find >>$ Maybe.fromJust

findIndex :: Foldable f => (a -> Bool) -> f a -> Maybe Int
findIndex f =
  Either.maybeRight . foldl (\i v -> case i of
                                Left i -> if f v then Right i else Left (succ i)
                                Right i -> Right i)
                            (Left 0)

unsafeFindIndex :: Foldable f => (a -> Bool) -> f a -> Int
unsafeFindIndex f = Maybe.fromJust . findIndex f

findWithIndex :: Foldable f => (a -> Bool) -> f a -> Maybe (a, Int)
findWithIndex f x =
  foldl (\(acc, i) v ->
            case acc of
              Just _ -> (acc, i)
              Nothing -> if f v then (Just v, i) else (Nothing, succ i))
        (Nothing, 0)
        x
  & \case
      (Nothing, _) -> Nothing
      (Just v, i) -> Just (v, i)

firstJust :: Foldable f => f (Maybe a) -> Maybe a
firstJust = Monad.join . Foldable.find Maybe.isJust

findJust :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
findJust f = Monad.join . fmap f . Foldable.find (Maybe.isJust . f)

unsafeFindJust :: Foldable f => (a -> Maybe b) -> f a -> b
unsafeFindJust = findJust >>$ Maybe.fromJust
