module AI where

import Game

type Score = Integer

data MoveScore = MoveScore Move Score
  deriving (Eq, Show)

instance Ord MoveScore where
  compare (MoveScore m1 s1) (MoveScore m2 s2) = compare s1 s2

moveScore :: Game -> Move -> MoveScore
moveScore game move = do
  let player = playerToMove game in do
    case makeMove game move of
      Left game -> MoveScore move (gameScore game player)
      _ -> MoveScore move (-1000)

gameScore :: Game -> Player -> Score
gameScore game player | winner game == Just player = 2
                      | winner game == Just (opponent player) = -1
                      | isDraw game = 1
                      | otherwise = 0

moveScores :: Game -> [MoveScore]
moveScores game = map (moveScore game) (validMoves game)

maximumMoveScore :: Game -> MoveScore
maximumMoveScore game = maximum (moveScores game)

getMove :: Game -> Move
getMove game = case (maximumMoveScore game) of
  MoveScore move score -> move
