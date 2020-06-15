module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model

    spec :: Spec
    spec = do
        describe "TestModel.Model" $ do
            it "getStringKeyPosition 1" $ do
                (getStringKeyPosition (Corda 1 0 (Tasto 1))) `shouldBe` (1 :: Int)
            it "getStringKeyPosition X" $ do
                (tasto (Corda 1 0 X)) `shouldBe` X             

