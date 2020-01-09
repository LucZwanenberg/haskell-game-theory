module Board where

import Game

data Board = Board (Row, Row, Row)
  deriving (Eq)

data Row = Row (Square, Square, Square)
  deriving (Eq)

data Square = Empty | PlayerOneMark | PlayerTwoMark
  deriving (Eq)

type PlayerToMove = Player

playerSquare :: Player -> Square
playerSquare PlayerOne = PlayerOneMark
playerSquare PlayerTwo = PlayerTwoMark

applyMoveToRow :: Row -> Player -> ColumnID -> Row
applyMoveToRow (Row (a, b, c)) player A = Row ((playerSquare player), b, c)
applyMoveToRow (Row (a, b, c)) player B = Row (a, (playerSquare player), c)
applyMoveToRow (Row (a, b, c)) player C = Row (a, b, (playerSquare player))

applyMoveToBoard :: Board -> Player -> Move -> Board
applyMoveToBoard (Board (a, b, c)) player (Move x One) = Board ((applyMoveToRow a player x), b, c)
applyMoveToBoard (Board (a, b, c)) player (Move x Two) = Board (a, (applyMoveToRow b player x), c)
applyMoveToBoard (Board (a, b, c)) player (Move x Three) = Board (a, b, (applyMoveToRow c player x))

initialBoard :: Board
initialBoard = Board
  (Row (Empty, Empty, Empty),
  Row (Empty, Empty, Empty),
  Row (Empty, Empty, Empty))

applyMovesToBoard :: Board -> PlayerToMove -> Moves -> Board
applyMovesToBoard board playerToMove (x:xs) = applyMovesToBoard (applyMoveToBoard board playerToMove x) (opponent playerToMove) xs
applyMovesToBoard board _ _ = board

deriveBoard :: Game -> Board
deriveBoard game = applyMovesToBoard initialBoard PlayerOne game

instance Show Square where
  show Empty = " "
  show PlayerOneMark = "X"
  show PlayerTwoMark = "O"

instance Show Row where
  show (Row (a, b, c)) = "[" ++ (show a) ++ "] [" ++ (show b) ++ "] [" ++ (show c) ++ "]"

instance Show Board where
  show (Board (one, two, three)) = "\n\n___________\n\n" ++ (show one) ++ "\n" ++ (show two) ++ "\n" ++ (show three) ++ "\n___________\n\n"
