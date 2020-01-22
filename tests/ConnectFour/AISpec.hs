module ConnectFour.AISpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import ConnectFour.AI as AI

instance (Arbitrary Analysis) where
  arbitrary = do
    type' <- choose (0 :: Int, 2 :: Int)
    case type' of
      0 -> do
        result <- elements [AI.P1Win, AI.Draw, AI.P2Win]
        moves <- choose (1, 3)
        return $ (Analysis (Left (result, moves)))
      1 -> do
        depth <- choose (1, 3)
        return $ (Analysis (Right (0, depth)))
      _ -> do
        score <- choose (-3, 3)
        depth <- choose (1, 3)
        return $ (Analysis (Right (score, depth)))

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

    describe "totally ordered set" $ do
      modifyMaxSuccess (const 1000) $ do
        it "is transitive" $
          property $
            \x y z -> (x :: Analysis) <= (y :: Analysis) && y <= (z :: Analysis) ==> x <= z

        it "is reflexive" $
          property $
            \x -> (x :: Analysis) <= x

        it "is anti-symmetric" $
          property $
            \x y -> not((x :: Analysis) <= (y :: Analysis) && y <= x) || x == y

        it "is comparable" $
          property $
            \x  y -> (x :: Analysis) <= (y :: Analysis) || y <= x

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




