module Either.Extra where

import qualified Data.Either as Either

getLeft :: Either a b -> a
getLeft (Left x) = x

getRight :: Either a b -> b
getRight (Right x) = x

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left x) = Just x
maybeLeft _ = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right x) = Just x
maybeRight _ = Nothing
