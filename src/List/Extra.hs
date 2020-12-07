module List.Extra where

(!!?) :: [a] -> Int -> Maybe a
(!!?) (x :  _) 0 = Just x
(!!?) (_ : xs) i = xs !!? pred i
(!!?) [] _ = Nothing

asPair :: [a] -> (a, a)
asPair [x, y] = (x, y)
