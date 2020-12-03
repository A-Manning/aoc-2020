module List where

(!!?) :: [a] -> Int -> Maybe a
(!!?)       [] _ = Nothing
(!!?) (x :  _) 0 = Just x
(!!?) (_ : xs) i = xs !!? pred i
