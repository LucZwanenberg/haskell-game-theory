module Helpers where

import Data.List

pairToList6 :: (a, a, a, a, a, a) -> [a]
pairToList6 (a0, a1, a2, a3, a4, a5) = [a0, a1, a2, a3, a4, a5]

listToPair7 :: [a] -> (a, a, a, a, a, a, a)
listToPair7 [a0, a1, a2, a3, a4, a5, a6] = (a0, a1, a2, a3, a4, a5, a6)

listToPair6 :: [a] -> (a, a, a, a, a, a)
listToPair6 [a0, a1, a2, a3, a4, a5] = (a0, a1, a2, a3, a4, a5)

pairToList7 :: (a, a, a, a, a, a, a) -> [a]
pairToList7 (a0, a1, a2, a3, a4, a5, a6) = [a0, a1, a2, a3, a4, a5, a6]

replaceInList :: [a] -> Int -> a -> [a]
replaceInList (x:xs) 0 a = [a] ++ xs
replaceInList (x:xs) i a = [x] ++ (replaceInList xs (i - 1) a)
replaceInList [] _ _ = []

findLastEmpty :: [Maybe a] -> Maybe Int
findLastEmpty items = findIndex isNothing (reverse [])

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex matcher items = do
  let index = findIndex matcher (reverse items) in
    case index of
      Just i -> Just (((length items) - 1) - i)
      _ -> Nothing

mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix transform matrix = map (map transform) matrix

combineArrays :: [a] -> [b] -> (a -> b -> c) -> [c]
combineArrays (a:as) (b:bs) combiner = [combiner a b] ++ (combineArrays as bs combiner)
combineArrays _ _ _ = []

combineMatrices :: [[a]] -> [[b]] -> (a -> b -> c) -> [[c]]
combineMatrices (aRow:aRows) (bRow:bRows) combiner = [combineArrays aRow bRow combiner] ++ (combineMatrices aRows bRows combiner)
combineMatrices _ _ _ = []

combine3Arrays :: [a] -> [b] -> [c] -> (a -> b -> c -> d) -> [d]
combine3Arrays (a:as) (b:bs) (c:cs) combiner = [combiner a b c] ++ (combine3Arrays as bs cs combiner)
combine3Arrays _ _ _ _ = []

combine3Matrices :: [[a]] -> [[b]] -> [[c]] -> (a -> b -> c -> d) -> [[d]]
combine3Matrices (aRow:aRows) (bRow:bRows) (cRow:cRows) combiner = [combine3Arrays aRow bRow cRow combiner] ++ (combine3Matrices aRows bRows cRows combiner)
combine3Matrices _ _ _ _ = []

combine4Arrays :: [a] -> [b] -> [c] -> [d] -> (a -> b -> c -> d -> e) -> [e]
combine4Arrays (a:as) (b:bs) (c:cs) (d:ds) combiner = [combiner a b c d] ++ (combine4Arrays as bs cs ds combiner)
combine4Arrays _ _ _ _ _ = []

combine4Matrices :: [[a]] -> [[b]] -> [[c]] -> [[d]] -> (a -> b -> c -> d -> e) -> [[e]]
combine4Matrices (aRow:aRows) (bRow:bRows) (cRow:cRows) (dRow:dRows) combiner = [combine4Arrays aRow bRow cRow dRow combiner] ++ (combine4Matrices aRows bRows cRows dRows combiner)
combine4Matrices _ _ _ _ _ = []

matrixSetValue :: [[a]] -> (Int, Int) -> a -> [[a]]
matrixSetValue matrix (x, y) val = do
  let newRow = replaceInList (matrix!!y) x val in
    replaceInList matrix y newRow

matrixGetValue :: [[a]] -> (Int, Int) -> a
matrixGetValue matrix (x, y) = (matrix!!y)!!x

rotateMatrixLeft :: [[x]] -> [[x]]
rotateMatrixLeft = transpose . map reverse

rotateMatrixRight :: [[a]] -> [[a]]
rotateMatrixRight = transpose . reverse
