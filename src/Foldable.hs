module Foldable where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

import Function

countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy f = length . filter f . Foldable.toList

count :: (Eq a, Foldable f) => a -> f a -> Int
count = countBy . (==)

unsafeFind :: Foldable f => (a -> Bool) -> f a -> a
unsafeFind = Foldable.find >>$ Maybe.fromJust

firstJust :: Foldable f => f (Maybe a) -> Maybe a
firstJust = Monad.join . Foldable.find Maybe.isJust
