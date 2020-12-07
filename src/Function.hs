module Function (
     module Control.Arrow
   , module Data.Function
   , (<<$)
   , ($>>)
   , (>>$)
   , ($<<)
   , (<<$$)
   , ($$>>)
   , (>>$$)
   , ($$<<)
) where

import Control.Arrow
import Data.Function ((&))

-- compose with the second input to an arrow
(<<$) :: Arrow a => a b (a d e) -> a c d -> a b (a c e)
(<<$) g f = (<<< f) ^<< g

-- compose with the second input to an arrow
($>>) :: Arrow a => a c d -> a b (a d e) -> a b (a c e)
($>>) = flip (<<$)

-- compose with an arrow of arity 2
(>>$) :: Arrow a => a b (a c d) -> a d e -> a b (a c e)
(>>$) f g = f >>^ (>>> g)

-- compose with an arrow of arity 2
($<<) :: Arrow a => a d e -> a b (a c d) -> a b (a c e)
($<<) = flip (>>$)

-- compose with the third input to an arrow
(<<$$) :: Arrow a => a b (a c (a e f)) -> a d e -> a b (a c (a d f))
(<<$$) g f = (<<$ f) ^<< g

-- compose with the third input to an arrow
($$>>) :: Arrow a => a d e -> a b (a c (a e f)) -> a b (a c (a d f))
($$>>) = flip (<<$$)

-- compose with an arrow of arity 3
(>>$$) :: Arrow a => a b (a c (a d e)) -> a e f -> a b (a c (a d f))
(>>$$) f g = f >>^ (>>$ g)

-- compose with an arrow of arity 3
($$<<) :: Arrow a => a e f -> a b (a c (a d e)) -> a b (a c (a d f))
($$<<) = flip (>>$$)
