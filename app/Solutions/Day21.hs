{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day21 where

import qualified Data.Maybe as Maybe
import qualified Either
import qualified List
import qualified Map
import qualified Matrix
import qualified Set
import qualified Vector

import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)

type Allergen = String
type Ingredient = String
type Label = (Set.T Ingredient, Set.T Allergen)

parseLabel :: String -> Label
parseLabel s =
  case List.splitOn " (contains " s of
    [nocontains] -> (Set.fromList $List.splitOn " " nocontains, Set.empty)
    [ingredients', rest] ->
      let ingredients = List.splitOn " " ingredients' in
      let allergens = List.splitOn ", " $reverse $tail $reverse rest in
      (Set.fromList ingredients, Set.fromList allergens)

solve :: String -> IO ()
solve = go . list parseLabel where
  go labels = do
    print $sum $map appearances $Set.toList notAllergens
    putStrLn $List.intercalate ","
             $map fst
             $List.sortOn snd
             $go' []
             $map (\i -> (i, candidateAllergens i))
             $Set.toList definitelyAllergens
    where
      allAllergens = foldr (Set.union . snd) Set.empty labels
      allIngredients = foldr (Set.union . fst) Set.empty labels
      notAllergen' i a = not $all (Set.member i . fst)
                             $filter (Set.member a . snd) labels
      notAllergen i = all (notAllergen' i) allAllergens
      appearances i = length $filter (Set.member i . fst) labels
      notAllergens = Set.filter notAllergen allIngredients
      definitelyAllergens = Set.difference allIngredients notAllergens
      filteredLabels = map (first $flip Set.difference notAllergens) labels
      candidateAllergen i a = all (Set.member i . fst)
                              $filter (Set.member a . snd) filteredLabels
      candidateAllergens i = Set.filter (candidateAllergen i) allAllergens
      go' acc [] = acc
      go' acc possibilities =
        let (ing, alrgn') = unsafeFind ((==) 1 . length . snd) possibilities in
        let [alrgn] = Set.toList alrgn' in
        let possibilities' = map (second (Set.delete alrgn))
                             $filter ((/=) ing . fst) possibilities in
        go' ((ing, alrgn):acc) possibilities'
