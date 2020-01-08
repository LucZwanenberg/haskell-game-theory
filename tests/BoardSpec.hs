module BoardSpec where

import Test.Hspec
import Prelude
import Board

spec :: Spec
spec = describe "helloWorld" $
  it "returns 'Hello World'" $
    (helloWorld 2) `shouldBe` "Hello World!"
