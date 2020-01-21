module ConnectFour.BoardSpec where

import Test.Hspec
import Prelude
import ConnectFour.Definitions
import ConnectFour.Game
import ConnectFour.Board

spec :: Spec
spec = do
  describe "#matrixSet" $ do
    it "sets correct element" $ do
      matrixSet [[0, 0], [0, 0]] 0 1 1 `shouldBe` [[0, 0], [1, 0]]

  describe "#gameToBoard" $ do
    context "empty game" $ do
      it "returns empty board" $ do
        showBoard (gameToBoard []) `shouldBe`
          " A   B   C   D   E   F   G\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]"

    context "game with single move" $ do
      it "returns correct board" $ do
        showBoard (gameToBoard [D]) `shouldBe`
          " A   B   C   D   E   F   G\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [X] [ ] [ ] [ ]"

    context "game with multiple moves" $ do
      it "returns correct board" $ do
        showBoard (gameToBoard [D, E, C, B, D, E]) `shouldBe`
          " A   B   C   D   E   F   G\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [X] [O] [ ] [ ]\n\
          \[ ] [O] [X] [X] [O] [ ] [ ]"

    context "game with multiple moves" $ do
      it "returns correct board" $ do
        showBoard (gameToBoard [D, D, D, D]) `shouldBe`
          " A   B   C   D   E   F   G\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [O] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [X] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [O] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [X] [ ] [ ] [ ]"

    context "game with multiple moves" $ do
      it "returns correct board" $ do
        showBoard (gameToBoard [D, E, C, B, D, E, C, D, D]) `shouldBe`
          " A   B   C   D   E   F   G\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [ ] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [X] [ ] [ ] [ ]\n\
          \[ ] [ ] [ ] [O] [ ] [ ] [ ]\n\
          \[ ] [ ] [X] [X] [O] [ ] [ ]\n\
          \[ ] [O] [X] [X] [O] [ ] [ ]"
