module TicTacToe.GameSpec where

import Test.Hspec
import Prelude
import TicTacToe.Game

spec :: Spec
spec = do
  describe "#checkValidGame" $ do
    context "empty game" $ do
      it "is a valid game" $ do
        isValidGame [] `shouldBe` True

    context "valid active game" $ do
      it "is valid game" $ do
        let game = [ Move A One
                   , Move B Two
                   , Move C Three
                   , Move B One ]
          in
            isValidGame game `shouldBe` True

    context "game with duplicate moves" $ do
      it "is not a valid game" $ do
        let move = Move A One
            game = [move, move]
          in
            isValidGame game `shouldBe` False

    context "game with moves after a win" $ do
      it "is not a valid game" $ do
        let game = [ Move A One
                  , Move B One
                  , Move A Two
                  , Move B Two
                  , Move A Three
                  , Move C Three ]
          in
            isValidGame game `shouldBe` False

  describe "#makeMove" $ do
    context "when space is unoccupied" $ do
      it "returns new game state" $ do
        let move = Move A One in
          makeMove [] move `shouldBe` (Left [move])

    context "when space is occupied" $ do
      it "returns AlreadyOccupied error" $ do
        let move = Move A One in
          makeMove [move] move `shouldBe` (Right AlreadyOccupied)
