module Chess.BoardSpec where

import Test.Hspec
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
