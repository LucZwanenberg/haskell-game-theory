module ConnectFour.Definitions where

data ColumnID = A | B | C | D | E | F | G
  deriving (Eq, Show)

type Move = ColumnID
type Game = [Move]

data Player = PlayerOne | PlayerTwo
  deriving (Eq, Show)

data Disc = Yellow | Red
  deriving (Eq, Show)

type Slot = Maybe Disc

type Board = (Row, Row, Row, Row, Row, Row)

type Row = (Slot, Slot, Slot, Slot, Slot, Slot, Slot)

data MoveError = ColumnIsFull | GameOver | InvalidGame
  deriving (Eq)

instance Show MoveError where
  show ColumnIsFull = "Column is full"
  show GameOver = "Game has finished"
  show InvalidGame = "Invalid game state"
