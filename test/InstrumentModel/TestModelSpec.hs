module InstrumentModel.TestModelSpec (spec) where

    import Test.Hspec
    import InstrumentModel.Model
    import Control.Monad

    spec :: Spec
    spec = do 
        describe "TestModel.Model" $ do
            testgetInstrumentString
            testGetNoMute
            testGetMinPos
            testGetMaxPos
            testFingerNote
            testFindPossibleFingeringsForChromaticInterval
            testFindChromaticIntervalFingeringsOnOtherStrings
            testGetFingeringsDistance
            testGetChordBaseString
            testSetFingeringOnFrettedInstrument
          

    testgetInstrumentString :: Spec
    testgetInstrumentString = do
        describe "" $ do
            it "getInstrumentStringKeyPosition 1" $ do
                (getInstrumentStringKeyPosition (InstrumentString 1 0 (Fret 1))) `shouldBe` (1 :: Int)
            it "getInstrumentStringKeyPosition X" $ do
                (fingeredFret (InstrumentString 1 0 X)) `shouldBe` X  


    testGetNoMute :: Spec
    testGetNoMute = do 
        describe "" $ do
            it "testGetNoMute  " $ do
                getNoMute testCase `shouldBe` expectedNoMuteStrings
                where 
                    testCase = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 1)), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    expectedNoMuteStrings = [(InstrumentString 2 7 (Fret 1))] 


    testGetMinPos :: Spec
    testGetMinPos = do 
        describe "" $ do
            it "testGetMinPos  " $ do
                getMinPos testCase `shouldBe` 2
                where 
                    testCase = [(InstrumentString 1 0 (Fret 5)), (InstrumentString 2 7 (Fret 3)), (InstrumentString 3 14 (Fret 2)), (InstrumentString 4 21 X)] 

    testGetMaxPos :: Spec
    testGetMaxPos = do 
        describe "" $ do
            it "testGetMaxPos  " $ do
                getMaxPos testCase `shouldBe` 5
                where 
                    testCase = [(InstrumentString 1 0 (Fret 5)), (InstrumentString 2 7 (Fret 3)), (InstrumentString 3 14 (Fret 2)), (InstrumentString 4 21 X)] 

    testFingerNote :: Spec
    testFingerNote = do 
        describe "" $ do
            it "testFingerNote  " $ do
                (fingerNote 2 (Fret 6) testCaseInit) `shouldBe` testCaseExpected
                where 
                    testCaseInit = [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    testCaseExpected = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 6)), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 


    fingeringTestCases :: [(String, [InstrumentString], InstrumentString, ChromaticInterval, [InstrumentString])]
    fingeringTestCases = [
        (
            "Terze maggiori",
            [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)],
            (InstrumentString 1 0 (Fret 5)),
            4 :: ChromaticInterval,
            [   (InstrumentString 1 0 X),
                (InstrumentString 2 7 (Fret 2)),
                (InstrumentString 3 14 (Fret 7)),
                -- (InstrumentString 4 21 (Fret 0))
                (InstrumentString 4 21 X)
                ] 
        ),  
        (
            "Terze minori",
            [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)],
            (InstrumentString 1 0 (Fret 5)),
            3 :: ChromaticInterval,
            [   (InstrumentString 1 0 X),
                (InstrumentString 2 7 (Fret 1)),
                (InstrumentString 3 14 (Fret 6)),
                (InstrumentString 4 21 (Fret 11))
                ] 
        ), 
        (
            "Quinte",
            [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)],
            (InstrumentString 1 0 (Fret 5)),
            7 :: ChromaticInterval,
            [   (InstrumentString 1 0 X),
                (InstrumentString 2 7 (Fret 5)),
                (InstrumentString 3 14 (Fret 10)),
                (InstrumentString 4 21 (Fret 3))
                ] 
        ),
        (
            "Seste",
            [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)],
            (InstrumentString 1 0 (Fret 5)),
            9 :: ChromaticInterval,
            [   (InstrumentString 1 0 X),
                (InstrumentString 2 7 (Fret 7)),
                (InstrumentString 3 14 X),
                (InstrumentString 4 21 (Fret 5))
                ] 
        )       
        ]

    testFindPossibleFingeringsForChromaticInterval :: Spec
    testFindPossibleFingeringsForChromaticInterval = do 
        forM_  
            fingeringTestCases $
            \(descr, instrument, fingering, interval, expected) -> 
                it descr $ do
                    (findPossibleFingeringsForChromaticInterval instrument fingering interval) `shouldBe` expected
                
 
    testFindChromaticIntervalFingeringsOnOtherStrings :: Spec
    testFindChromaticIntervalFingeringsOnOtherStrings = do 
        describe "" $ do
            it "testFindChromaticIntervalFingeringsOnOtherStrings  " $ do
                (findChromaticIntervalFingeringsOnOtherStrings baseInstrument firstFingering interval) `shouldBe` fingeringsExpected
                where 
                    baseInstrument = [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    firstFingering = (InstrumentString 1 0 (Fret 5))
                    interval=9
                    fingeringsExpected = [InstrumentString {stringNumber = 1, stringTuning = 0, fingeredFret = X},InstrumentString {stringNumber = 2, stringTuning = 7, fingeredFret = Fret 7},InstrumentString {stringNumber = 3, stringTuning = 14, fingeredFret = X},InstrumentString {stringNumber = 4, stringTuning = 21, fingeredFret = Fret 5}] 

    testGetFingeringsDistance :: Spec
    testGetFingeringsDistance = do 
        describe "" $ do
            it "testGetFingeringsDistance  " $ do
                (getFingeringsDistance (InstrumentString 1 0 (Fret 2)) (InstrumentString 1 0 (Fret 5))) `shouldBe` 3
            it "testGetFingeringsDistance  " $ do
                (getFingeringsDistance (InstrumentString 1 0 (Fret 7)) (InstrumentString 1 0 (Fret 3))) `shouldBe` 4

    testGetChordBaseString :: Spec
    testGetChordBaseString = do 
        describe "" $ do
            it "testGetChordBaseString  " $ do
                (getChordBaseString baseInstrument) `shouldBe` expectedString
                where 
                    baseInstrument = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 5)), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    expectedString = (InstrumentString 2 7 (Fret 5))

    testSetFingeringOnFrettedInstrument :: Spec              
    testSetFingeringOnFrettedInstrument = do 
        describe "" $ do
            it "testSetFingeringOnFrettedInstrument  " $ do
                (setFingeringOnFrettedInstrument baseInstrument instrumentString) `shouldBe` expectedInstrument
                where 
                    baseInstrument = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 5)), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
                    instrumentString = Just (InstrumentString 3 14 (Fret 6))
                    expectedInstrument = [(InstrumentString 1 0 X), (InstrumentString 2 7 (Fret 5)), (InstrumentString 3 14 (Fret 6)), (InstrumentString 4 21 X)] 
