module BoardSpec where

import Test.Hspec
import Prelude
import Game
import Board

spec :: Spec
spec = do
  describe "#deriveBoard" $ do
    context "when no moves are made" $ do
      it "returns empty board" $ do
        deriveBoard [] `shouldBe` Board
          (Row (Empty, Empty, Empty),
          Row (Empty, Empty, Empty),
          Row (Empty, Empty, Empty))

    context "when moves are made" $ do
      it "marks moves for players" $ do
        let game = [ Move B Two
                   , Move A One
                   , Move B Three
                   , Move B One ]
          in
            deriveBoard game `shouldBe` Board
              (Row (PlayerTwoMark,  PlayerTwoMark,  Empty),
              Row (Empty,           PlayerOneMark,  Empty),
              Row (Empty,           PlayerOneMark,  Empty))
