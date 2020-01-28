module ConnectFour.AISpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import ConnectFour.Definitions as Definitions
import ConnectFour.AI as AI

genResult = do
  result <- elements [P1Win, NoWin, P2Win]
  return $ result

genMoves = do
  moves <- choose (1, 3)
  return $ moves

genDeductiveAnalysis = do
  result <- genResult
  moves <- genMoves
  return $ DeductiveAnalysis (result, moves)

instance (Arbitrary DeductiveAnalysis) where
  arbitrary = do
    deductiveAnalysis <- genDeductiveAnalysis
    return $ deductiveAnalysis

genScore = do
  score <- choose (-2, 2)
  return $ score

genDepth = do
  depth <- choose (1, 3)
  return $ depth

genHeuristicAnalysis = do
  score <- genScore
  depth <- genDepth
  return $ HeuristicAnalysis (score, depth)

instance (Arbitrary HeuristicAnalysis) where
  arbitrary = do
    heuristicAnalysis <- genHeuristicAnalysis
    return $ heuristicAnalysis

genMixedAnalysis = do
  type' <- elements ["deductive", "heuristic"]
  case type' of
    "deductive" -> do
      deductiveAnalysis <- genDeductiveAnalysis
      (return $ MixedAnalysis (Left deductiveAnalysis))
    "heuristic" -> do
      heuristicAnalysis <- genHeuristicAnalysis
      (return $ MixedAnalysis (Right heuristicAnalysis))

instance (Arbitrary MixedAnalysis) where
  arbitrary = do
    mixedAnalysis <- genMixedAnalysis
    return $ mixedAnalysis

spec :: Spec
spec = do
  describe "DeductiveAnalysis" $ do
    describe "#show" $ do
      context "P1 win" $ do
        it "shows correct string" $ do
          show (DeductiveAnalysis (P1Win, 3)) `shouldBe` "P1 win in 3"

      context "draw" $ do
        it "shows correct string" $ do
          show (DeductiveAnalysis (NoWin, 3)) `shouldBe` "Draw in 3"

      context "P2 win" $ do
        it "shows correct sting" $ do
          show (DeductiveAnalysis (P2Win, 3)) `shouldBe` "P2 win in 3"

    describe "#score" $ do
      context "P1 win" $ do
        it "returns max bound" $ do
          score (DeductiveAnalysis (P1Win, 3)) `shouldBe` maxBound

      context "Draw" $ do
        it "returns 0" $ do
          score (DeductiveAnalysis (NoWin, 3)) `shouldBe` 0

      context "P2 win" $ do
        it "returns min bound" $ do
          score (DeductiveAnalysis (P2Win, 3)) `shouldBe` minBound

    describe "ordering" $ do
      it "quick deductive P1 win is greater than slow deductive P1 win" $ do
        (DeductiveAnalysis (P1Win, 2)) > (DeductiveAnalysis (P1Win, 6)) `shouldBe` True

      it "slow deductive P1 win is greater than slow deductive no win" $ do
        (DeductiveAnalysis (P1Win, 6)) > (DeductiveAnalysis (NoWin, 6)) `shouldBe` True

      it "slow deductive no win is greater than fast deductive no win" $ do
        (DeductiveAnalysis (NoWin, 6)) > (DeductiveAnalysis (NoWin, 2)) `shouldBe` True

      it "fast deductive  no win is greater than slow deductive P2 win" $ do
        (DeductiveAnalysis (NoWin, 2)) > (DeductiveAnalysis (P2Win, 2)) `shouldBe` True

      it "slow deductive P2 win is greater than fast deductive P2 win" $ do
        (DeductiveAnalysis (P2Win, 6)) > (DeductiveAnalysis (P2Win, 2)) `shouldBe` True

      modifyMaxSuccess (const 1000) $ do
        it "is transitive" $
          property $
            \x y z -> (x :: DeductiveAnalysis) <= (y :: DeductiveAnalysis) && y <= (z :: DeductiveAnalysis) ==> x <= z

        it "is reflexive" $
          property $
            \x -> (x :: DeductiveAnalysis) <= x

        it "is anti-symmetric" $
          property $
            \x y -> not((x :: DeductiveAnalysis) <= (y :: DeductiveAnalysis) && y <= x) || x == y

        it "is comparable" $
          property $
            \x  y -> (x :: DeductiveAnalysis) <= (y :: DeductiveAnalysis) || y <= x

  describe "HeuristicAnalysis" $ do
    describe "#show" $ do
      it "shows correct string" $ do
        show (HeuristicAnalysis (3, 12)) `shouldBe` "Score: 3, Depth: 12"

    describe "#score" $ do
      it "returns score" $ do
        score (HeuristicAnalysis (3, 12)) `shouldBe` 3

    describe "ordering" $ do
      it "high heuristic score is greater than low heuristic score" $ do
        (HeuristicAnalysis (10, 6)) > (HeuristicAnalysis (3, 2)) `shouldBe` True

      modifyMaxSuccess (const 1000) $ do
        it "is transitive" $
          property $
            \x y z -> (x :: HeuristicAnalysis) <= (y :: HeuristicAnalysis) && y <= (z :: HeuristicAnalysis) ==> x <= z

        it "is reflexive" $
          property $
            \x -> (x :: HeuristicAnalysis) <= x

        it "is anti-symmetric" $
          property $
            \x y -> not((x :: HeuristicAnalysis) <= (y :: HeuristicAnalysis) && y <= x) || x == y

        it "is comparable" $
          property $
            \x  y -> (x :: HeuristicAnalysis) <= (y :: HeuristicAnalysis) || y <= x

    describe "MixedAnalysis" $ do
      describe "ordering" $ do
        it "slow deductive p1 win is greater than high heuristic score" $ do
          mixedAnalysis (DeductiveAnalysis (P1Win, 6)) > mixedAnalysis (HeuristicAnalysis (10, 3)) `shouldBe` True

        it "neutral heuristic score is greater than deductive no win" $ do
          mixedAnalysis (HeuristicAnalysis (0, 3)) > mixedAnalysis (DeductiveAnalysis (NoWin, 6)) `shouldBe` True

        it "deductive no win is greater than heuristic negative score" $ do
          mixedAnalysis (DeductiveAnalysis (NoWin, 6)) > mixedAnalysis(HeuristicAnalysis(-5, 2)) `shouldBe` True

        modifyMaxSuccess (const 1000) $ do
          it "is transitive" $
            property $
              \x y z -> (x :: MixedAnalysis) <= (y :: MixedAnalysis) && y <= (z :: MixedAnalysis) ==> x <= z

          it "is reflexive" $
            property $
              \x -> (x :: MixedAnalysis) <= x

          it "is anti-symmetric" $
            property $
              \x y -> not((x :: MixedAnalysis) <= (y :: MixedAnalysis) && y <= x) || x == y

          it "is comparable" $
            property $
              \x  y -> (x :: MixedAnalysis) <= (y :: MixedAnalysis) || y <= x

    describe "#bestMove" $ do
      describe "tactics" $ do
        context "can win this move" $ do
          it "will connect four" $ do
            depthBestMove 1 [B, C, B, C, B, C] `shouldBe` B
        context "cannot win this move" $ do
          context "opponent could connect four next four if not blocked" $ do
            it "will block" $ do
              depthBestMove 2 [B, C, B, C, B] `shouldBe` B
        it "will setup a multi-directional attack" $ do
          depthBestMove 3 [B, B, D, D] `shouldBe` C
        it "will prevent a multi-directional attack" $ do
          elem (depthBestMove 4 [C, C, E]) [B, D, F] `shouldBe` True

    describe "#criticalSquareMatrix" $ do
      it "marks critical squares" $ do
        criticalSquareMatrix [D, D, C, E, E, F, E, F, F, G, D, F, G, E, B, D] `shouldBe`
          [ [AI.NotCritical,  AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical]
          , [AI.NotCritical,  AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical, AI.NotCritical]
          , [AI.NotCritical,  AI.NotCritical, AI.CriticalP2,  AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.CriticalBoth]
          , [AI.NotCritical,  AI.NotCritical, AI.CriticalP1,  AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.CriticalP1]
          , [AI.NotCritical,  AI.NotCritical, AI.NotCritical, AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.Occupied]
          , [AI.CriticalP1,   AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.Occupied,    AI.Occupied]]
