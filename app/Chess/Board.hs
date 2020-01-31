module Chess.Board where

import Helpers
import Chess.Definitions

fileCount :: Int
fileCount = 8

rankCount :: Int
rankCount = 8

emptyBoard :: Board
emptyBoard = take rankCount (repeat (take fileCount (repeat Nothing)))

initialBoard :: Board
initialBoard = setSquares emptyBoard
  [ ((1,8), Just (Black, Rook))
  , ((2,8), Just (Black, Knight))
  , ((3,8), Just (Black, Bishop))
  , ((4,8), Just (Black, Queen))
  , ((5,8), Just (Black, King))
  , ((6,8), Just (Black, Bishop))
  , ((7,8), Just (Black, Knight))
  , ((8,8), Just (Black, Rook))

  , ((1,7), Just (Black, Pawn))
  , ((2,7), Just (Black, Pawn))
  , ((3,7), Just (Black, Pawn))
  , ((4,7), Just (Black, Pawn))
  , ((5,7), Just (Black, Pawn))
  , ((6,7), Just (Black, Pawn))
  , ((7,7), Just (Black, Pawn))
  , ((8,7), Just (Black, Pawn))

  , ((1,2), Just (White, Pawn))
  , ((2,2), Just (White, Pawn))
  , ((3,2), Just (White, Pawn))
  , ((4,2), Just (White, Pawn))
  , ((5,2), Just (White, Pawn))
  , ((6,2), Just (White, Pawn))
  , ((7,2), Just (White, Pawn))
  , ((8,2), Just (White, Pawn))

  , ((1,1), Just (White, Rook))
  , ((2,1), Just (White, Knight))
  , ((3,1), Just (White, Bishop))
  , ((4,1), Just (White, Queen))
  , ((5,1), Just (White, King))
  , ((6,1), Just (White, Bishop))
  , ((7,1), Just (White, Knight))
  , ((8,1), Just (White, Rook))]

setSquares :: Board -> [(Position, Maybe Piece)] -> Board
setSquares board positions = foldl (\board (position, square) -> do
  (setSquare board position square)) board positions

setSquare :: Board -> Position -> (Maybe Piece) -> Board
setSquare board position = matrixSetValue board (positionToXY position)

getSquare :: Board -> Position -> Square
getSquare board position = matrixGetValue board (positionToXY position)

positionToXY :: Position -> (Int, Int)
positionToXY (file, rank) = (file - 1, rankCount - rank)
