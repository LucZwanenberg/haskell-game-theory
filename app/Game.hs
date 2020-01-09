module Game where

data ColumnID = A | B | C
  deriving (Eq, Show)

data RowID = One | Two | Three
  deriving (Eq, Show)

data Move = Move ColumnID RowID
  deriving (Eq, Show)

type Moves = [Move]

data Player = PlayerOne | PlayerTwo
  deriving (Eq, Show)

type Game = [Move]

type PlayerMoves = [Move]

data GameState = Active | PlayerOneWon | PlayerTwoWon | Draw
  deriving (Eq, Show)

data MoveError = AlreadyOccupied | GameOver | InvalidGame
  deriving (Eq, Show)


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
