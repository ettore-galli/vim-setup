module InstrumentModel.TestModel where

    import Test.Hspec
    import InstrumentModel.Model

    testModel :: Spec
    testModel = do
        describe "TestModel.Model" $ do
            it "getPosTasto" $ do
                (getPosTasto (Corda 1 0 (Tasto 1))) `shouldBe` (1 :: Int)