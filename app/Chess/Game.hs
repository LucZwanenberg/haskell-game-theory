module Chess.Game where

import Chess.Definitions
import Chess.Board
import Chess.Move

gameToBoard :: Game -> Board
gameToBoard moves = foldl (applyMove) initialBoard moves
