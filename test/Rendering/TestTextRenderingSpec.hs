module Rendering.TestTextRenderingSpec (spec) where

    import Test.Hspec

    import Core.Model
    import Core.ChordCalculator
    import Rendering.TextRendering


    spec :: Spec
    spec = do 
        describe "Test module InstrumentModel.Model" $ do
            testRenderCapoName
             

    testRenderCapoName :: Spec
    testRenderCapoName = do
        describe "testRenderCapoName" $ do
            it "1" $ do
                (renderCapoName 1) `shouldBe` "1st"
            it "2" $ do
                (renderCapoName 2) `shouldBe` "2nd"
            it "3" $ do
                (renderCapoName 3) `shouldBe` "3rd"
            it "4" $ do
                (renderCapoName 4) `shouldBe` "4th"                
 