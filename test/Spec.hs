import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Exercise C" $ do
      it "returns the original number when given a positive input" $
        1 `shouldBe` 2
