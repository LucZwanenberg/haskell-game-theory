module ConnectFour.AI where

import ConnectFour.Definitions
import ConnectFour.Game

class Analysis a where
  score :: a -> Int
  mixedAnalysis :: a -> MixedAnalysis

-- Deductive analysis
data GameResult = P1Win | NoWin | P2Win
  deriving (Eq)
type NumberOfMoves = Int

data DeductiveAnalysis = DeductiveAnalysis (GameResult, NumberOfMoves)
  deriving (Eq)

instance Show DeductiveAnalysis where
  show (DeductiveAnalysis (P1Win, moves)) = "P1 win in " ++ (show moves)
  show (DeductiveAnalysis (NoWin, moves)) = "Draw in " ++ (show moves)
  show (DeductiveAnalysis (P2Win, moves)) = "P2 win in " ++ (show moves)

instance Analysis DeductiveAnalysis where
  score (DeductiveAnalysis (P1Win, _)) = maxBound
  score (DeductiveAnalysis (NoWin, _)) = 0
  score (DeductiveAnalysis (P2Win, _)) = minBound
  mixedAnalysis a = MixedAnalysis (Left a)

moves :: DeductiveAnalysis -> Int
moves (DeductiveAnalysis (_, x)) = x

instance Ord DeductiveAnalysis where
  compare a b | scoreA /= scoreB = compare scoreA scoreB
              | scoreA > 0 = compare movesB movesA
              | otherwise = compare movesA movesB
            where
                scoreA = score a
                movesA = moves a
                scoreB = score b
                movesB = moves b

-- Heuristic analysis
type Depth = Int
type Score = Int

data HeuristicAnalysis = HeuristicAnalysis (Score, Depth)
  deriving (Eq)

instance Show HeuristicAnalysis where
  show (HeuristicAnalysis (score, depth)) = "Score: " ++ (show score) ++ ", Depth: " ++ (show depth)

instance Analysis HeuristicAnalysis where
  score (HeuristicAnalysis (score, _)) = score
  mixedAnalysis a = MixedAnalysis (Right a)

instance Ord HeuristicAnalysis where
  compare (HeuristicAnalysis (scoreA, movesA)) (HeuristicAnalysis (scoreB, movesB))
    | scoreA /= scoreB = compare scoreA scoreB
    | scoreA > 0 = compare movesB movesA
    | otherwise = compare movesA movesB

-- Mixed analysis
data MixedAnalysis = MixedAnalysis (Either DeductiveAnalysis HeuristicAnalysis)
  deriving (Show, Eq)

compareMixedAnalysis :: MixedAnalysis -> MixedAnalysis -> Ordering
compareMixedAnalysis (MixedAnalysis (Left a)) (MixedAnalysis (Left b)) = compare a b
compareMixedAnalysis (MixedAnalysis (Right a)) (MixedAnalysis (Right b)) = compare a b
compareMixedAnalysis (MixedAnalysis (Left a)) ((MixedAnalysis (Right b))) = do
  case (a, b) of
    ((DeductiveAnalysis (result, _)), HeuristicAnalysis (score, _)) -> do
      case result of
        P1Win -> compare 1 0
        NoWin -> do
          let result | score >= 0 = compare 0 1
                     | otherwise = compare 1 0 in
            result
        P2Win -> compare 0 1
compareMixedAnalysis a b = if b <= a then (compare 1 0) else (compare 0 1)

instance Ord MixedAnalysis where
  compare a b = compareMixedAnalysis a b

-- Analyze moves
data MoveScore = MoveScore Move MixedAnalysis
  deriving (Eq, Show)

instance Ord MoveScore where
  compare (MoveScore _ a1) (MoveScore _ a2) = compare a1 a2

getMove :: Game -> Move
getMove game = case bestMove game of
  MoveScore move score -> move

bestMove :: Game -> MoveScore
bestMove game = case playerToMove game of
  PlayerOne -> maximum ( analyzeMoves game )
  PlayerTwo -> minimum ( analyzeMoves game )

analyzeMoves :: Game -> [MoveScore]
-- TODO: implement analyze moves
analyzeMoves game = [(A (mixedAnalysis (HeuristicAnalysis(12, 2))))]
