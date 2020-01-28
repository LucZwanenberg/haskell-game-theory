module ConnectFour.Game where

import ConnectFour.Definitions
import ConnectFour.Board as Board
import Data.List

winner :: Game -> Maybe Player
winner game | arrayContains sumValues 4 = Just PlayerOne
            | arrayContains sumValues (-4) = Just PlayerTwo
            | otherwise = Nothing
          where
            sumValues = map sum (subarraysLength4 (analysisMatrix game ))

arrayContains :: Eq a => [a] -> a -> Bool
arrayContains (x:xs) item | x == item = True
                          | otherwise = arrayContains xs item
arrayContains _ item = False

showGame :: Game -> [Char]
showGame game = showBoard (gameToBoard game)

hasWinner :: Game -> Bool
hasWinner game = case winner game of
  Nothing -> False
  _ -> True

isDraw :: Game -> Bool
isDraw game | hasWinner game = False
          | length game < (7 * 6) = False
          | otherwise = True

gameOver :: Game -> Bool
gameOver game | hasWinner game = True
              | isDraw game = True
              | otherwise = False

gameState :: Game -> GameState
gameState game | winner game == Just PlayerOne = PlayerOneWon
               | winner game == Just PlayerTwo = PlayerTwoWon
               | isDraw game = Draw
               | otherwise = Active

isValidGame :: Game -> Bool
isValidGame (x:xs) | isValidGame (init (x:xs)) == False = False
                   | isValidMove (init (x:xs)) (last (x:xs)) == False = False
                   | otherwise = True
isValidGame _ = True

count :: [a] -> (a -> Bool) -> Integer
count (x:xs) matcher | matcher x = (count xs matcher) + 1
                     | otherwise = count xs matcher
count _ _ = 0

makeMove :: Game -> Move -> Either Game MoveError
makeMove game move | gameOver game = Right GameOver
                   | columnIsFull game move = Right ColumnIsFull
                   | otherwise = Left (game ++ [move])

validMoves :: Game -> [Move]
validMoves game = (isValidMove game) `filter` ([A, B, C, D, E, F, G])

isValidMove :: Game -> Move -> Bool
isValidMove game move | gameOver game = False
                      | columnIsFull game move = False
                      | otherwise = True

columnIsFull :: Game -> ColumnID -> Bool
columnIsFull game column = (count game (== column) >= 6 )

gameToBoard :: Game -> Board
gameToBoard game = gameToBoard' emptyBoard game PlayerOne

gameToMatrix :: Game -> [[Slot]]
gameToMatrix game = boardToMatrix ( gameToBoard game )

analysisMatrix :: Game -> [[Int]]
analysisMatrix game = transformMatrix analysisValue (gameToMatrix game)

mapWithIndex :: [a] -> (Int -> a -> b) -> [b]
mapWithIndex items transform = mapWithGivenIndex 0 items transform

mapWithGivenIndex :: Int -> [a] -> (Int -> a -> b) -> [b]
mapWithGivenIndex index (x:xs) transform = [transform index x] ++ (mapWithGivenIndex (index + 1) xs transform)
mapWithGivenIndex _ _ _ = []

labeledAnalysisMatrix :: Game -> [[(ColumnNumber, RowNumber, Int)]]
labeledAnalysisMatrix game = do
  mapWithIndex (analysisMatrix game)
    (\rowNumber row -> do
      (mapWithIndex row (\columnNumber value -> (columnNumber, rowNumber, value)))
    )

analysisValue :: Slot -> Int
analysisValue (Just Yellow) = 1
analysisValue (Just Red) = -1
analysisValue Nothing = 0

transformMatrix :: (a -> b) -> [[a]] -> [[b]]
transformMatrix transform matrix = map (map transform) matrix

subarraysLength4 :: [[a]] -> [[a]]
subarraysLength4 matrix = foldl (++) [] (map (contiguousSubarrays 4) (subarraysAllDirections matrix))

subarraysAllDirections :: [[a]] -> [[a]]
subarraysAllDirections matrix = (horizontalSubarrays matrix) ++ (verticalSubarrays matrix) ++ (diagonalSubarrays matrix)

contiguousSubarrays :: Int -> [a] -> [[a]]
contiguousSubarrays length items  = contiguousSubarrays' length items []

contiguousSubarrays' :: Int -> [a] -> [[a]] -> [[a]]
contiguousSubarrays' leng items result | (length result) + leng > (length items) = result
                                       | otherwise = contiguousSubarrays' leng items (result ++ [take leng (drop (length result) items)])

horizontalSubarrays :: [[a]] -> [[a]]
horizontalSubarrays matrix = matrix

verticalSubarrays :: [[a]] -> [[a]]
verticalSubarrays matrix = rotl matrix

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

diagonalSubarrays :: [[a]] -> [[a]]
diagonalSubarrays matrix = (diagonalSubarrays' matrix) ++ (diagonalSubarrays'' matrix)

diagonalSubarrays' :: [[a]] -> [[a]]
diagonalSubarrays' matrix = diagonals matrix

diagonalSubarrays'' :: [[a]] -> [[a]]
diagonalSubarrays'' matrix = diagonals (rotl matrix)

diagonals = map concat
          . transpose
          . zipWith (\ns xs -> ns ++ map (:[]) xs)
                    (iterate ([]:) [])

playerToMove :: Game -> Player
playerToMove game | (length game) `mod` 2 == 0 = PlayerOne
                  | otherwise = PlayerTwo
