module Board where

data Space = Empty | P1 | P2
type Board =
  (
    (Space, Space, Space),
    (Space, Space, Space),
    (Space, Space, Space)
  )

data Column = A | B | C
data Row = One | Two | Three

data Move = Move Column Row
type Game = [Move]

data MoveError = AlreadyOccupied | GameOver

makeMove :: Game -> Move -> Either Game MoveError
makeMove game move = Right AlreadyOccupied -- TODO: check for duplicates

helloWorld :: Int -> String
helloWorld x = "Hello World!"
