module Chess.Move where

import Chess.Definitions
import Chess.Board

applyMove :: ValidBoard -> ValidMove -> ValidBoard
applyMove board (startPos, endPos, maybePromotion) = do
  let originalSquare = getSquare board startPos
      newSquare = maybePromote originalSquare maybePromotion
    in
      setSquares board
        [ (startPos, Nothing)
        , (endPos, newSquare) ]

maybePromote :: Square -> (Maybe Promotion) -> Square
maybePromote (Just (color, _)) (Just newType) = Just (color, newType)
maybePromote square _ = square
