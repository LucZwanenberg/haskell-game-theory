module ConnectFour.GameSpec where

import Test.Hspec
import Prelude
import ConnectFour.Definitions
import ConnectFour.Game
import ConnectFour.Board

spec :: Spec
spec = do
  describe "#winner" $ do
    context "without winner" $ do
      it "returns Nothing" $ do
        winner [] `shouldBe` Nothing

    context "when player one won" $ do
      it "should return PlayerOne" $ do
        winner [A, B, A, C, A, D, A] `shouldBe` Just PlayerOne

  describe "#isValidGame" $ do
    context "empty game" $ do
      it "is valid" $ do
        isValidGame [] `shouldBe` True

    context "active game" $ do
      it "is valid" $ do
        isValidGame [D, D, C, B, D, D] `shouldBe` True

    context "won game" $ do
      it "is valid" $ do
        isValidGame [D, D, C, B, E, E, F] `shouldBe` True

    context "game with at max 6 moves in the same column" $ do
      it "is not valid" $ do
        isValidGame [A, A, A, B, C, A, A, A, G, D, B] `shouldBe` True

    context "game with more than 6 moves in same column" $ do
      it "is not valid" $ do
        isValidGame [A, A, A, B, C, A, A, A, G, A, B] `shouldBe` False

    context "game with moves after win" $ do
      it "is not valid" $ do
        isValidGame [D, D, C, B, E, E, F, A] `shouldBe` False

  describe "#contiguousSubarrays" $ do
    context "when given length is greater than the length of array" $ do
      it "returns the empty array" $ do
        contiguousSubarrays 5 [1, 2, 3] `shouldBe` []

    context "when given length is equal to length of array" $ do
      it "returns only the given array" $ do
        contiguousSubarrays 3 [1, 2, 3] `shouldMatchList` [[1, 2, 3]]

    context "when given length is smaller than length of array" $ do
      it "returns all subarrays of given length" $ do
        contiguousSubarrays 4 [1, 2, 4, 3, 2, 6] `shouldMatchList` [[1, 2, 4, 3], [2, 4, 3, 2], [4, 3, 2, 6]]

  describe "#horizontalSubarrays" $ do
    it "returns given arrays" $ do
      horizontalSubarrays [[0, 1, 2], [3, 4, 5], [6, 7, 8]] `shouldMatchList` [[0, 1, 2], [3, 4, 5], [6, 7, 8]]

  describe "#verticalSubarrays" $ do
    it "returns vertical arrays" $ do
      verticalSubarrays [[0, 1, 2], [3, 4, 5], [6, 7, 8]] `shouldMatchList` [[0, 3, 6], [1, 4, 7], [2, 5, 8]]

  describe "#diagonalSubarrays" $ do
    it "returns diagonal arrays" $ do
      diagonalSubarrays [[0, 1, 2], [3, 4, 5], [6, 7, 8]] `shouldMatchList` [[0], [1, 3], [2, 4, 6], [5, 7], [8], [2], [5, 1], [8, 4, 0], [7, 3], [6]]
