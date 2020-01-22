module TestSuiteSpec ( module TestSuiteSpec ) where

import Test.Hspec
import Prelude
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

spec :: Spec
spec = do
  describe "hspec" $
    it "can run tests" $
      1 + 1 `shouldBe` 2

  describe "quick check" $ do
    it "can run test" $
      property $
        \x -> (read . show) x == (x :: Int)
