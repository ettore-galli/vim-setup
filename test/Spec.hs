import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

import InstrumentModel.Model

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "getPosTasto" $ do
      (getPosTasto (Corda 1 0 (Tasto 1))) `shouldBe` (1 :: Int)