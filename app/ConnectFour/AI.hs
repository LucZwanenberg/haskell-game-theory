module ConnectFour.AI where

import Helpers

import ConnectFour.Definitions
import ConnectFour.Game
import ConnectFour.Board

filterLeft :: [Either a b] -> [a]
filterLeft ((Left x):xs) = [x] ++ (filterLeft xs)
filterLeft (_:xs) = filterLeft xs
filterLeft _ = []

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
data MoveAnalysis = MoveAnalysis Move MixedAnalysis
  deriving (Eq, Show)

instance Ord MoveAnalysis where
  compare (MoveAnalysis _ a1) (MoveAnalysis _ a2) = compare a1 a2

getMove :: Game -> Move
getMove game = bestMove game

bestMove :: Game -> Move
bestMove = depthBestMove 4

depthBestMove :: Depth -> Game -> Move
depthBestMove depth game = case (bestMoveAnalysis game depth) of
  MoveAnalysis move score -> move

bestMoveAnalysis :: Game -> Depth -> MoveAnalysis
bestMoveAnalysis game depth = case playerToMove game of
  PlayerOne -> maximum ( analyzeMoves game depth )
  PlayerTwo -> minimum ( analyzeMoves game depth )

analyzeMoves :: Game -> Depth -> [MoveAnalysis]
analyzeMoves game depth = filterLeft ((analyzeMove game depth) `map` (validMoves game))

analyzeMove :: Game -> Depth -> Move -> (Either MoveAnalysis MoveError)
analyzeMove game depth move = do
  case (makeMove game move) of
    Left game -> do
      case (deductiveAnalysisDepth0 game) of
        Just a -> Left ( MoveAnalysis move (mixedAnalysis (increaseMoves a)))
        Nothing -> do
          let result | depth > 1 = do
                        case (bestMoveAnalysis game (depth - 1)) of
                          (MoveAnalysis submove analysis) -> Left (MoveAnalysis move (increaseDepthMove analysis))
                     | otherwise = Left ( MoveAnalysis move (mixedAnalysis (increaseDepth (heuristicAnalysisDepth0 game)))) in
            result
    Right moveError -> Right moveError

deductiveAnalysisDepth0 :: Game -> (Maybe DeductiveAnalysis)
deductiveAnalysisDepth0 game = do
  case (gameState game) of
    PlayerOneWon -> Just (DeductiveAnalysis (P1Win, 0))
    Draw -> Just (DeductiveAnalysis (NoWin, 0))
    PlayerTwoWon -> Just (DeductiveAnalysis (P2Win, 0))
    Active -> Nothing

increaseDepthMove :: MixedAnalysis -> MixedAnalysis
increaseDepthMove mixedAnalysis = do
  case mixedAnalysis of
    (MixedAnalysis (Left a)) -> (MixedAnalysis (Left (increaseMoves a)))
    (MixedAnalysis (Right a)) -> (MixedAnalysis (Right (increaseDepth a)))

increaseMoves :: DeductiveAnalysis -> DeductiveAnalysis
increaseMoves (DeductiveAnalysis (result, moves)) = DeductiveAnalysis (result, moves + 1)

increaseDepth :: HeuristicAnalysis -> HeuristicAnalysis
increaseDepth (HeuristicAnalysis (score, depth)) = HeuristicAnalysis (score, depth + 1)

heuristicAnalysisDepth0 :: Game -> HeuristicAnalysis
heuristicAnalysisDepth0 game = HeuristicAnalysis ((analysisSubarrays game), 0)

analysisSubarrays :: Game -> Score
analysisSubarrays game = sum (map scoreSubarray (subarraysLength4 (analysisMatrix game)))

scoreSubarray :: [Int] -> Score
scoreSubarray array = do
  case (analyzeSubarray array) of
    (3, 0) -> 10
    (0, 3) -> -10
    (2, 0) -> 5
    (0, 2) -> -5
    (1, 0) -> 1
    (0, 1) -> -1
    _ -> 0

analyzeSubarray :: [Int] -> (Int, Int)
analyzeSubarray (x:xs) = do
  case (analyzeSubarray xs) of
    (p1, p2) -> do
      case x of
        1 -> (p1 + 1, p2)
        -1 -> (p1, p2 + 1)
        _ -> (p1, p2)
analyzeSubarray _ = (0, 0)

data CriticalSquareMatrixValue = Occupied | CriticalP1 | CriticalP2 | CriticalBoth | NotCritical
  deriving (Eq, Show)

initCriticalMatrixRow = [NotCritical, NotCritical, NotCritical, NotCritical, NotCritical, NotCritical, NotCritical]
initCriticalMatrix = [initCriticalMatrixRow, initCriticalMatrixRow, initCriticalMatrixRow, initCriticalMatrixRow, initCriticalMatrixRow, initCriticalMatrixRow]

emptyCriticalMatrixRow :: [Maybe CriticalSquareMatrixValue]
emptyCriticalMatrixRow = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

emptyCritialSquareMatrix :: [[Maybe CriticalSquareMatrixValue]]
emptyCritialSquareMatrix = [emptyCriticalMatrixRow, emptyCriticalMatrixRow, emptyCriticalMatrixRow, emptyCriticalMatrixRow, emptyCriticalMatrixRow, emptyCriticalMatrixRow]

setOccupiedSquares :: [[CriticalSquareMatrixValue]] -> Game -> [[CriticalSquareMatrixValue]]
setOccupiedSquares matrix game = combineMatrices matrix (gameToMatrix game) (\a b -> do
    case b of
      Nothing -> a
      _ -> Occupied
  )

occupiedSpacesMatrix :: Game -> [[(Maybe CriticalSquareMatrixValue)]]
occupiedSpacesMatrix game = do
  mapMatrix (\x -> do
      case x of
        Just _ -> Just Occupied
        Nothing -> Nothing
    ) (gameToMatrix game)

p1CriticalMatrix :: Game -> [[(Maybe CriticalSquareMatrixValue)]]
p1CriticalMatrix game = do
  foldl (\x y -> do
      case y of
        [(_, _, 1), (_, _, 1), (_, _, 1), (c, r, 0)] -> matrixSetValue x c r (Just CriticalP1)
        [(_, _, 1), (_, _, 1), (c, r, 0), (_, _, 1)] -> matrixSetValue x c r (Just CriticalP1)
        [(_, _, 1), (c, r, 0), (_, _, 1), (_, _, 1)] -> matrixSetValue x c r (Just CriticalP1)
        [(c, r, 0), (_, _, 1), (_, _, 1), (_, _, 1)] -> matrixSetValue x c r (Just CriticalP1)
        _ -> x
    ) emptyCritialSquareMatrix (subarraysLength4 ( labeledAnalysisMatrix game ))

p2CriticalMatrix :: Game -> [[(Maybe CriticalSquareMatrixValue)]]
p2CriticalMatrix game = do
  foldl (\x y -> do
      case y of
        [(_, _, -1), (_, _, -1), (_, _, -1), (c, r, 0)] -> matrixSetValue x c r (Just CriticalP2)
        [(_, _, -1), (_, _, -1), (c, r, 0), (_, _, -1)] -> matrixSetValue x c r (Just CriticalP2)
        [(_, _, -1), (c, r, 0), (_, _, -1), (_, _, -1)] -> matrixSetValue x c r (Just CriticalP2)
        [(c, r, 0), (_, _, -1), (_, _, -1), (_, _, -1)] -> matrixSetValue x c r (Just CriticalP2)
        _ -> x
    ) emptyCritialSquareMatrix (subarraysLength4 ( labeledAnalysisMatrix game ))

criticalSquareMatrix :: Game -> [[CriticalSquareMatrixValue]]
criticalSquareMatrix game = do
  let initMatrix = initCriticalMatrix
      osMatrix = occupiedSpacesMatrix game
      p1Matrix = p1CriticalMatrix game
      p2Matrix = p2CriticalMatrix game in
        combine4Matrices initMatrix osMatrix p1Matrix p2Matrix (\a b c d -> combineValues a [b, c, d])

combineValues :: CriticalSquareMatrixValue -> [Maybe CriticalSquareMatrixValue] -> CriticalSquareMatrixValue
combineValues a (x:xs) = combineValues (combineValue a x) xs
combineValues a _ = a

combineValue :: CriticalSquareMatrixValue -> (Maybe CriticalSquareMatrixValue) -> CriticalSquareMatrixValue
combineValue CriticalP1 (Just CriticalP2) = CriticalBoth
combineValue CriticalP2 (Just CriticalP1) = CriticalBoth
combineValue CriticalP1 _ = CriticalP1
combineValue _ (Just CriticalP1) = CriticalP1
combineValue CriticalP2 _ = CriticalP2
combineValue _ (Just CriticalP2) = CriticalP2
combineValue Occupied _ = Occupied
combineValue _ (Just Occupied) = Occupied
combineValue a _ = a
