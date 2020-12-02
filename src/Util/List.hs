module Util.List where

(!!?) :: [a] -> Int -> Maybe a
(!!?)       [] _ = Nothing
(!!?) (x :  _) 0 = Just x
(!!?) (_ : xs) i = xs !!? pred i

countBy :: (a -> Bool) -> [a] -> Int
countBy f = length . filter f

count :: Eq a => a -> [a] -> Int
count = countBy . (==)

firstJust :: [Maybe a] -> Maybe a
firstJust             [] = Nothing
firstJust ( Just x :  _) = Just x
firstJust (Nothing : xs) = firstJust xs
