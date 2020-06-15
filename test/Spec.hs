import Test.Hspec
import InstrumentModel.TestModel

main :: IO ()
main = hspec $ testModel
  --describe "Prelude.head" $ do
  --  it "getPosTastoooooo" $ do
  --    (getPosTasto (Corda 1 0 (Tasto 1))) `shouldBe` (1 :: Int)