module TicTacToe.Game where

data ColumnID = A | B | C
  deriving (Eq)

instance Show ColumnID where
  show A = "A"
  show B = "B"
  show C = "C"

data RowNumber = One | Two | Three
  deriving (Eq)

instance Show RowNumber where
  show One = "1"
  show Two = "2"
  show Three = "3"

data Move = Move ColumnID RowNumber
  deriving (Eq)

instance Show Move where
  show (Move column row) = (show column) ++ (show row)

type Moves = [Move]

data Player = PlayerOne | PlayerTwo
  deriving (Eq)

instance Show Player where
  show PlayerOne = "Player 1"
  show PlayerTwo = "Player 2"

type Game = [Move]

type PlayerMoves = [Move]

data GameState = Active | PlayerOneWon | PlayerTwoWon | Draw
  deriving (Eq)

instance Show GameState where
  show Active = "Active"
  show PlayerOneWon = "Player 1 has won!"
  show PlayerTwoWon = "Player 2 has won!"
  show Draw = "It's a draw!"

data MoveError = AlreadyOccupied | GameOver
  deriving (Eq)

instance Show MoveError where
  show AlreadyOccupied = "Square is already occupied"
  show GameOver = "Game has finished"

winConditions =
  [ [ Move A One    , Move A Two    , Move A Three  ]
  , [ Move B One    , Move B Two    , Move B Three  ]
  , [ Move C One    , Move C Two    , Move C Three  ]

  , [ Move A One    , Move B One    , Move C One    ]
  , [ Move A Two    , Move B Two    , Move C Two    ]
  , [ Move A Three  , Move B Three  , Move C Three  ]

  , [ Move A One    , Move B Two    , Move C Three  ]
  , [ Move A Three  , Move B Two    , Move C One    ] ]

gameContainsMove :: Game -> Move -> Bool
gameContainsMove game move = move `elem` game

movesForPlayer :: Game -> Player -> PlayerMoves
movesForPlayer (p1:p2:xs) PlayerOne = [p1] ++ (movesForPlayer xs PlayerOne)
movesForPlayer (p1:p2:xs) PlayerTwo = [p2] ++ (movesForPlayer xs PlayerTwo)
movesForPlayer (p1:[]) PlayerOne = [p1]
movesForPlayer _ _ = []

xs `supersetOf` ys = null $ filter (not . (`elem` xs)) ys

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

threeInARow :: PlayerMoves -> Bool
threeInARow moves = foldl (||) False (map (supersetOf moves) winConditions)

winner :: Game -> Maybe Player
winner game | threeInARow ( game `movesForPlayer` PlayerOne ) = Just PlayerOne
            | threeInARow ( game `movesForPlayer` PlayerTwo ) = Just PlayerTwo
            | otherwise = Nothing

hasWinner :: Game -> Bool
hasWinner game = case winner game of
  Nothing -> False
  _ -> True

isDraw :: Game -> Bool
isDraw game | hasWinner game = False
            | length game < 9 = False
            | otherwise = True

gameOver :: Game -> Bool
gameOver game | isDraw game = True
              | hasWinner game = True
              | otherwise = False

isValidMove :: Game -> Move -> Bool
isValidMove game move | gameOver game = False
                      | gameContainsMove game move = False
                      | otherwise = True
playerToMove :: Game -> Player
playerToMove game | (length game) `mod` 2 == 0 = PlayerOne
                  | otherwise = PlayerTwo

allMoves :: [Move]
allMoves = [ Move A One
  , Move A Two
  , Move A Three
  , Move B One
  , Move B Two
  , Move B Three
  , Move C One
  , Move C Two
  , Move C Three ]

validMoves :: Game -> [Move]
validMoves game = filter (isValidMove game) allMoves

isValidGame :: Game -> Bool
isValidGame (x:xs) | isValidGame (init (x:xs)) == False = False
                   | isValidMove (init (x:xs)) (last (x:xs)) == False = False
                   | otherwise = True
isValidGame _ = True

spaceIsOccupied :: Game -> Move -> Bool
spaceIsOccupied game move = move `elem` game

makeMove :: Game -> Move -> Either Game MoveError
makeMove game move | gameOver game = Right GameOver
                   | spaceIsOccupied game move = Right AlreadyOccupied
                   | otherwise = Left (game ++ [move])

gameState :: Game -> GameState
gameState game | winner game == Just PlayerOne = PlayerOneWon
               | winner game == Just PlayerTwo = PlayerTwoWon
               | isDraw game = Draw
               | otherwise = Active
