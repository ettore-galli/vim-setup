module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model

    spec :: Spec
    spec = do
        describe "TestModel.Model" $ do
            it "getStringKeyPosition 1" $ do
                (getStringKeyPosition (InstrumentString 1 0 (Fret 1))) `shouldBe` (1 :: Int)
            it "getStringKeyPosition X" $ do
                (tasto (InstrumentString 1 0 X)) `shouldBe` X             

