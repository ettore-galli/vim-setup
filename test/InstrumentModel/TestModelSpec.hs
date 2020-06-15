module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model

    spec :: Spec
    spec = do
        describe "TestModel.Model" $ do
            it "getInstrumentStringKeyPosition 1" $ do
                (getInstrumentStringKeyPosition (InstrumentString 1 0 (Fret 1))) `shouldBe` (1 :: Int)
            it "getInstrumentStringKeyPosition X" $ do
                (fingeredFret (InstrumentString 1 0 X)) `shouldBe` X             

