module ConnectFour.AI where

import ConnectFour.Definitions

data GameResult = P1Win | Draw | P2Win
  deriving (Eq)

type HeuristicAnalysisScore = Int

type NumberOfMoves = Int
type Depth = Int

data Analysis = Analysis (Either (GameResult, NumberOfMoves) (HeuristicAnalysisScore, Depth))
  deriving (Eq)

instance Show Analysis where
  show (Analysis (Left (P1Win, nrOfMoves))) =
    "P1 win in " ++ (show nrOfMoves)

  show (Analysis (Left (ConnectFour.AI.Draw, nrOfMoves))) =
    "Draw in " ++ (show nrOfMoves)

  show (Analysis (Left (P2Win, nrOfMoves))) =
    "P2 win in " ++ (show nrOfMoves)

  show (Analysis (Right (score, depth))) = "Score: " ++ (show score) ++ ", Depth: " ++ (show depth)

quantifyAnalysis :: Analysis -> Int
quantifyAnalysis (Analysis (Left (P1Win, moves))) = maxBound
quantifyAnalysis (Analysis (Left (ConnectFour.AI.Draw, moves))) = 0
quantifyAnalysis (Analysis (Left (P2Win, moves))) = minBound
quantifyAnalysis (Analysis (Right (score, depth))) = score

instance Ord Analysis where
  compare a b
      | aScore /= bScore = compare aScore bScore
      | otherwise = do
        case (a, b) of
          (Analysis (Left (ConnectFour.AI.P1Win, aMoves)), Analysis (Left (ConnectFour.AI.P1Win, bMoves))) -> compare bMoves aMoves
          (Analysis (Left (ConnectFour.AI.Draw, aMoves)), Analysis (Left (ConnectFour.AI.Draw, bMoves))) -> compare aMoves bMoves
          (Analysis (Left (ConnectFour.AI.Draw, aMoves)), Analysis (Right (0, bMoves))) -> compare 0 1
          (Analysis (Right (0, aMoves)), Analysis (Left (ConnectFour.AI.Draw, bMoves))) -> compare 1 0
          (Analysis (Right (0, aMoves)), Analysis (Right (0, bMoves))) -> compare bMoves aMoves
          (Analysis (Left (ConnectFour.AI.P2Win, aMoves)), Analysis (Left (ConnectFour.AI.P2Win, bMoves))) -> compare aMoves bMoves
          _ -> compare 0 1
    where
      aScore = quantifyAnalysis a
      bScore = quantifyAnalysis b
