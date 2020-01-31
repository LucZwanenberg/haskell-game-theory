module Chess.Definitions where

-- Game state definitions
type File = Int
type Rank = Int
type Position = (File, Rank)
type StartPosition = Position
type EndPosition = Position
type Promotion = PieceType
type Move = (StartPosition, EndPosition, Maybe Promotion)
type Moves = [Move]
type Game = Moves

-- Board definitions
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

type Piece = (Color, PieceType)
type Square = Maybe Piece
type Board = [[Square]]

-- Meta
data MoveError = MoveErrorReason -- TODO
type ValidMove = Move
type ValidGame = Game
type ValidBoard = Board
