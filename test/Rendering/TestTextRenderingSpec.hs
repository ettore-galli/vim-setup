module Rendering.TestTextRenderingSpec (spec) where

    import Test.Hspec

    import Core.Model
    import Core.ChordCalculator
    import Rendering.TextRendering


    spec :: Spec
    spec = do 
        describe "Test module InstrumentModel.Model" $ do
            testRenderCapoName
            testRenderMuteOrSeparator 
            testRenderSingleString
            testRenderInstrumentFingering


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
 

    testRenderMuteOrSeparator :: Spec
    testRenderMuteOrSeparator = do
        describe "testRenderMuteOrSeparator" $ do
            it "Unstrummed" $ do
                (renderMuteOrSeparator Nothing) `shouldBe` "X "
            it "Oper string" $ do
                (renderMuteOrSeparator (Just 0)) `shouldBe` "0 "                
            it "Any fingering" $ do
                (renderMuteOrSeparator (Just 1)) `shouldBe` "  "


    testRenderSingleString :: Spec
    testRenderSingleString = do
        describe "testRenderSingleString" $ do
            it "Unstrummed" $ do
                (renderSingleString Nothing  2 5) `shouldBe` "X |---|---|---|---"
            it "Open String" $ do
                (renderSingleString (Just 0) 2 5) `shouldBe` "0 |---|---|---|---"
            it "Fretted" $ do
                (renderSingleString (Just 3) 2 5) `shouldBe` "  |---|-o-|---|---"             


    testRenderInstrumentFingering :: Spec
    testRenderInstrumentFingering = do
        describe "testRenderInstrumentFingering" $ do
            it "Example 1" $ do
                (
                    renderInstrumentFingering 
                    [(TunedString 7 (Just 1)), (TunedString 0 (Just 2)), (TunedString 4 (Just 3)), (TunedString 9 (Just 4))]
                    1 6
                    ) `shouldBe` "  |-o-|---|---|---|---|---\n  |---|-o-|---|---|---|---\n  |---|---|-o-|---|---|---\n  |---|---|---|-o-|---|---\n"
           