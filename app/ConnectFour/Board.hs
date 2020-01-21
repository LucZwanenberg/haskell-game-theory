module ConnectFour.Board where

import ConnectFour.Definitions

opposingPlayer :: Player -> Player
opposingPlayer PlayerOne = PlayerTwo
opposingPlayer PlayerTwo = PlayerOne

discForPlayer :: Player -> Disc
discForPlayer PlayerOne = Yellow
discForPlayer PlayerTwo = Red

header = " A   B   C   D   E   F   G\n"

showSlot :: Slot -> [Char]
showSlot Nothing = "[ ]"
showSlot (Just Yellow) = "[X]"
showSlot (Just Red) = "[O]"
showRow (a0, a1, a2, a3, a4, a5, a6) = (showSlot a0) ++ " " ++ (showSlot a1) ++ " " ++ (showSlot a2) ++ " " ++ (showSlot a3) ++ " " ++ (showSlot a4) ++ " " ++ (showSlot a5) ++ " " ++ (showSlot a6)
showBoard (a0, a1, a2, a3, a4, a5) = header ++ (showRow a0) ++ "\n" ++ (showRow a1) ++ "\n" ++ (showRow a2) ++ "\n" ++ (showRow a3) ++ "\n" ++ (showRow a4) ++ "\n" ++ (showRow a5)

columnIndex :: ColumnID -> Int
columnIndex A = 0
columnIndex B = 1
columnIndex C = 2
columnIndex D = 3
columnIndex E = 4
columnIndex F = 5
columnIndex G = 6

emptyBoard :: Board
emptyBoard = (emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, emptyRow)

emptyRow :: Row
emptyRow = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

gameToBoard' :: Board -> Game -> Player -> Board
gameToBoard' board (x:xs) player = gameToBoard' (dropDisc (discForPlayer player) x board) xs (opposingPlayer player)
gameToBoard' board _ player = board

rowSlotIsEmpty :: ColumnID -> [Slot] -> Bool
-- TODO: use zip to avoid runtime errors?
rowSlotIsEmpty column slots = (slots!!(columnIndex column)) == Nothing

matrixSet :: [[a]] -> Int -> Int -> a -> [[a]]
-- TODO: use zip to avoid runetime errors?
matrixSet matrix x y value = replaceInList matrix y (replaceInList ( matrix!!y ) x value)

dropDisc :: Disc -> ColumnID -> Board -> Board
dropDisc disc column board = boardMatrixTransformation board (dropDisc' disc column)

dropDisc' ::  Disc -> ColumnID -> [[Slot]] -> [[Slot]]
dropDisc' disc column matrix = do
  let ci = (columnIndex column) in
    let mri = findLastIndex (rowSlotIsEmpty column) matrix  in
      case mri of
        Just ri -> matrixSet matrix ci ri (Just disc)
        _ -> matrix

boardMatrixTransformation :: Board -> ([[Slot]] -> [[Slot]]) -> Board
boardMatrixTransformation board transform = matrixToBoard ( transform ( boardToMatrix board) )

matrixToBoard :: [[Slot]] -> Board
matrixToBoard slots = listToPair6 (map listToRow slots)

boardToMatrix :: Board -> [[Slot]]
boardToMatrix board = map rowToList (pairToList6 board)

rowToList :: Row -> [Slot]
rowToList row = pairToList7 row

listToRow :: [Slot] -> Row
listToRow slots = listToPair7 slots

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

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex matcher (x:xs) | matcher x = Just 0
                         | otherwise = do
                           let index = findIndex matcher xs in
                             case index of
                               Just i -> Just (i + 1)
                               _ -> Nothing
findIndex _ _ = Nothing


