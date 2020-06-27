module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model
    import Control.Monad

    spec :: Spec
    spec = do 
        describe "Test module InstrumentModel.Model" $ do
            testTestsDoRun

    testTestsDoRun :: Spec
    testTestsDoRun = do
        describe "Tests are running" $ do
            it "Basic run test" $ do
                1 `shouldBe` 1
