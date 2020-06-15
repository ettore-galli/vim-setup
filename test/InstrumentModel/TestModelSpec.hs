module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model

    spec :: Spec
    spec = do 
        testgetInstrumentString
        testGetNoMute
          

    testgetInstrumentString :: Spec
    testgetInstrumentString = do
        describe "TestModel.Model" $ do
            it "getInstrumentStringKeyPosition 1" $ do
                (getInstrumentStringKeyPosition (InstrumentString 1 0 (Fret 1))) `shouldBe` (1 :: Int)
            it "getInstrumentStringKeyPosition X" $ do
                (fingeredFret (InstrumentString 1 0 X)) `shouldBe` X  


    testGetNoMute :: Spec
    testGetNoMute = do 
        describe "TestModel.Model" $ do
            it "testGetNoMute  " $ do
                getNoMute testGetNoMuteCase `shouldBe` expectedNoMuteStrings
                where 
                    testGetNoMuteCase = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 1)), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    expectedNoMuteStrings = [(InstrumentString 2 7 (Fret 1))] 

