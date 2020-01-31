module Chess.BoardSpec where

import Test.Hspec
import Chess.Game
import Chess.Definitions
import Chess.ShowBoard
import Chess.Board

spec :: Spec
spec = do
  describe "#gameToBoard" $ do
    context "empty game" $ do
      it "returns initial board" $ do
        showBoard (gameToBoard []) `shouldBe`
          "[♜][♞][♝][♛][♚][♝][♞][♜]\n\
          \[♟][♟][♟][♟][♟][♟][♟][♟]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[♙][♙][♙][♙][♙][♙][♙][♙]\n\
          \[♖][♘][♗][♕][♔][♗][♘][♖]"

    context "game with single move" $ do
      it "returns correct board" $ do
        showBoard (gameToBoard [((5, 2), (5, 4), Nothing)]) `shouldBe`
          "[♜][♞][♝][♛][♚][♝][♞][♜]\n\
          \[♟][♟][♟][♟][♟][♟][♟][♟]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[ ][ ][ ][ ][♙][ ][ ][ ]\n\
          \[ ][ ][ ][ ][ ][ ][ ][ ]\n\
          \[♙][♙][♙][♙][ ][♙][♙][♙]\n\
          \[♖][♘][♗][♕][♔][♗][♘][♖]"
