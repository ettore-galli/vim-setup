module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model

    spec :: Spec
    spec = do
        describe "TestModel.Model" $ do
            it "getPosTasto" $ do
                (getPosTasto (Corda 1 0 (Tasto 1))) `shouldBe` (1 :: Int)