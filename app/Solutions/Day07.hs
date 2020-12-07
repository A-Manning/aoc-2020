module Solutions.Day07 where

import qualified Map
import qualified Set

import Input hiding (get)
import List (mapAccumR, splitOn)
import Map ((!))

readRule :: String -> (String, [(String, Int)])
readRule s =
  let [bag, rest] = splitOn " bags contain " s in
  case rest of
    "no other bags." -> (bag, [])
    _ -> (bag, map parseContains $ splitOn ", " rest)
  where parseContains s0 =
          let (d, (' ':rest0)) = head $ readsPrec 0 s0 in
          let (bag0:_) = splitOn " bag" rest0 in
          (bag0, d)

contentsMap :: [(String, [(String, Int)])] -> Map.T String (Set.T String)
contentsMap rules = fst $ mapAccumR go Map.empty $ map fst rules where
  rulesMap = Map.map (map fst) $ Map.fromList rules
  go memo k =
    let (memo', sets) = mapAccumR lookupMemo memo $ rulesMap ! k in
    let res = foldr1 Set.union $ Set.fromList (rulesMap ! k) : sets in
    (Map.insert k res memo', res)
  lookupMemo memo k =
    if Map.member k memo then (memo, memo ! k) else
    go memo k

canContain :: String -> [(String, [(String, Int)])] -> Int
canContain bag =
  Map.size . Map.filter (Set.member bag) . contentsMap

bagsInside :: [(String, [(String, Int)])] -> Map.T String Int
bagsInside rules = fst $ mapAccumR go Map.empty $ map fst rules where
  rulesMap = Map.fromList rules
  go memo k =
    let (memo', counts) = mapAccumR lookupMemo memo $ map fst $ rulesMap ! k in
    let res = sum $ zipWith (\c n -> succ c * n) counts
                  $ map snd $ rulesMap ! k in
    (Map.insert k res memo', res)
  lookupMemo memo k =
    if Map.member k memo then (memo, memo ! k) else
    go memo k

solve :: String -> IO ()
solve = go . list readRule where
  go rules = do
    print $ Map.size $ Map.filter (Set.member "shiny gold") $ contentsMap rules
    print $ bagsInside rules ! "shiny gold"
