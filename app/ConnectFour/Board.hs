module ConnectFour.Board where

import Helpers

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

type ColumnNumber = Int
columnNumber :: ColumnID -> ColumnNumber
columnNumber A = 0
columnNumber B = 1
columnNumber C = 2
columnNumber D = 3
columnNumber E = 4
columnNumber F = 5
columnNumber G = 6

emptyBoard :: Board
emptyBoard = (emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, emptyRow)

emptyRow :: Row
emptyRow = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

gameToBoard' :: Board -> Game -> Player -> Board
gameToBoard' board (x:xs) player = gameToBoard' (dropDisc (discForPlayer player) x board) xs (opposingPlayer player)
gameToBoard' board _ player = board

rowSlotIsEmpty :: ColumnID -> [Slot] -> Bool
-- TODO: use zip to avoid runtime errors?
rowSlotIsEmpty column slots = (slots!!(columnNumber column)) == Nothing

matrixSet :: [[a]] -> Int -> Int -> a -> [[a]]
-- TODO: use zip to avoid runetime errors?
matrixSet matrix x y value = replaceInList matrix y (replaceInList ( matrix!!y ) x value)

dropDisc :: Disc -> ColumnID -> Board -> Board
dropDisc disc column board = boardMatrixTransformation board (dropDisc' disc column)

dropDisc' ::  Disc -> ColumnID -> [[Slot]] -> [[Slot]]
dropDisc' disc column matrix = do
  let ci = (columnNumber column) in
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



