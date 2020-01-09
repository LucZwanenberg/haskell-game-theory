module AI where

import Game

type Score = Integer

data MoveScore = MoveScore Move Score
  deriving (Eq, Show)

instance Ord MoveScore where
  compare (MoveScore m1 s1) (MoveScore m2 s2) = compare s1 s2

filterLeft :: [Either a b] -> [a]
filterLeft ((Left x):xs) = [x] ++ (filterLeft xs)
filterLeft (_:xs) = filterLeft xs
filterLeft _ = []

getMove :: Game -> Move
getMove game = case perfectPlay game of
  MoveScore move score -> move

perfectPlay :: Game -> MoveScore
perfectPlay game = case playerToMove game of
  PlayerOne -> maximum ( analyzeMoves game )
  PlayerTwo -> minimum ( analyzeMoves game )

analyzeMoves :: Game -> [MoveScore]
analyzeMoves game = filterLeft ( map (analyzeMove game) (validMoves game) )

analyzeMove :: Game -> Move -> Either MoveScore MoveError
analyzeMove game move = case makeMove game move of
  Left game -> Left (MoveScore move (score game))
  Right moveError -> Right moveError

score :: Game -> Score
score game | winner game == Just PlayerOne = 1
           | isDraw game = 0
           | winner game == Just PlayerTwo = -1
           | otherwise = case perfectPlay game of
              MoveScore move score -> score
