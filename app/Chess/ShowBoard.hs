module Chess.ShowBoard where

import Chess.Definitions
import Chess.Board

concatLines :: [[Char]] -> [Char]
concatLines items = foldl (\a b -> a ++ (if length a > 0 then "\n" else "") ++ b) "" items

showBoard :: Board -> [Char]
showBoard board = concatLines (showRank `map` board)

showRank :: [Square] -> [Char]
showRank rank = concat (showSquare `map` rank)

showSquare :: Square -> [Char]
showSquare (Just piece) = "[" ++ (showPiece piece) ++ "]"
showSquare Nothing = "[ ]"

showPiece :: Piece -> [Char]
showPiece (White, King)     = "♔"
showPiece (White, Queen)    = "♕"
showPiece (White, Rook)     = "♖"
showPiece (White, Bishop)   = "♗"
showPiece (White, Knight)   = "♘"
showPiece (White, Pawn)     = "♙"
showPiece (Black, King)     = "♚"
showPiece (Black, Queen)    = "♛"
showPiece (Black, Rook)     = "♜"
showPiece (Black, Bishop)   = "♝"
showPiece (Black, Knight)   = "♞"
showPiece (Black, Pawn)     = "♟"
