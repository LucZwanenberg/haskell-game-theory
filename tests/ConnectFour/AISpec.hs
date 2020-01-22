module ConnectFour.AISpec where

import Test.Hspec
import ConnectFour.AI as AI

spec :: Spec
spec = do
  describe "Analysis" $ do
    describe "#show" $ do

      context "win" $ do
        it "shows correct string" $ do
          show ( Analysis (Left (AI.P1Win, 3))) `shouldBe` "P1 win in 3"

      context "draw" $ do
        it "shows correct string" $ do
          show ( Analysis (Left (AI.Draw, 3))) `shouldBe` "Draw in 3"

      context "loss" $ do
        it "shows correct string" $ do
          show ( Analysis (Left (AI.P2Win, 3))) `shouldBe` "P2 win in 3"

      context "heuristic evaluation" $ do
        it "shows score" $ do
          show ( Analysis (Right (123, 10))) `shouldBe` "Score: 123, Depth: 10"

    describe "ordering" $ do
      it "fast guaranteed P1 win > slow guaranteed P1 win" $ do
        (Analysis (Left (AI.P1Win, 1))) > (Analysis (Left (AI.P1Win, 10))) `shouldBe` True

      it "slow guaranteed P1 win > positive high heuristic score" $ do
        (Analysis (Left (AI.P1Win, 10)) > (Analysis (Right (1000, 1)))) `shouldBe` True

      it "positive high heuristic score > positive low heuristic score" $ do
        (Analysis (Right (1000, 1))) > (Analysis (Right (1, 1))) `shouldBe` True

      it "positive low heuristic score > neutral fast heuristic score" $ do
        (Analysis (Right (1, 1))) > (Analysis (Right (0, 1))) `shouldBe` True

      it "neutral fast heuristic score > neutral slow heuristic score" $ do
        (Analysis (Right (0, 1))) > (Analysis (Right (0, 10))) `shouldBe` True

      it "neutral slow heuristic score > guaranteed slow draw" $ do
        (Analysis (Right (0, 10))) > (Analysis (Left (AI.Draw, 10))) `shouldBe` True

      it "guaranteed slow draw > guaranteed fast draw" $ do
        (Analysis (Left (AI.Draw, 10))) > (Analysis (Left (AI.Draw, 1))) `shouldBe` True

      it "guaranteed fast draw > negative higher score" $ do
        (Analysis (Left (AI.Draw, 1))) > (Analysis (Right (-1, 1))) `shouldBe` True

      it "negative higher score > negative lower score" $ do
        (Analysis (Right (-1, 1))) > (Analysis (Right (-1000, 1))) `shouldBe` True

      it "negative lower score > slow guaranteed P2 win" $ do
        (Analysis (Right (-1000, 1))) > (Analysis (Left (AI.P2Win, 1000))) `shouldBe` True

      it "slow guaranteed P2 win > fast guaranteed P2 win" $ do
        (Analysis (Left (AI.P2Win, 10))) > (Analysis (Left (AI.P2Win, 1))) `shouldBe` True




