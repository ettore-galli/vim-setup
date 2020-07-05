module Core.TestChordCalculatorSpec (spec) where

    import Test.Hspec
    import Control.Monad

    import Core.Model
    import Core.ChordCalculator

    spec :: Spec
    spec = do 
        describe "Test module InstrumentModel.Model" $ do
            testApplyFingeringOnTunedString
            testPrepareFingerings
            testApplyFingeringsOnInstrument
            testParseInstrumentFromTunings
            testParseFingering
            testParseFingeringsFromList
            testGetMinPosition
            testGetMaxPosition
            testCalculateFingeringforInterval
            testFindFingeringForIntervalOnTunedString

    fingeringTestCases :: [(String, TunedString, Fingering, TunedString)]
    fingeringTestCases = [
            ("No fingering", (TunedString 0 Nothing), Nothing, (TunedString 0 Nothing)),
            ("First Just", (TunedString 0 Nothing), (Just 1), (TunedString 0 (Just 1))),
            ("Change Just", (TunedString 0 (Just 1)), (Just 2), (TunedString 0 (Just 2)))
        ]

    testApplyFingeringOnTunedString :: Spec
    testApplyFingeringOnTunedString = do
        describe "testApplyFingeringOnTunedString" $
            forM_  
                fingeringTestCases $
                \(descr, string, fingering, expected) -> 
                    it descr $ do
                        (applyFingeringOnTunedString string fingering) `shouldBe` expected


    prepareFingeringTestCases :: [(String, Instrument, Int, Fingering, [Fingering])]
    prepareFingeringTestCases = [
            (
                "String 1", 
                [(TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing)],
                1, 
                (Just 5),
                [(Just 5), Nothing, Nothing, Nothing]
                ),
            (    
                "String 2", [(TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing)],
                2, 
                (Just 5),
                [Nothing, (Just 5), Nothing, Nothing]
                ),
            (    
                "String 3", [(TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing)],
                3, 
                (Just 5),
                [Nothing, Nothing, (Just 5), Nothing]
                ),
            (    
                "String 4", 
                [(TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing), (TunedString 0 Nothing)],
                4, 
                (Just 5),
                [Nothing, Nothing, Nothing, (Just 5)]
                )                                
        ]

    testPrepareFingerings :: Spec
    testPrepareFingerings = do
        describe "testPrepareFingerings" $
            forM_  
                prepareFingeringTestCases $
                \(descr, instrument, strn, fingering, expected) -> 
                    it descr $ do
                        (prepareFingerings instrument strn fingering) `shouldBe` expected
 

    instrumentFingeringTestCases :: [(String, Instrument, [Fingering], Instrument)]
    instrumentFingeringTestCases = [
        (
            "String 1", 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
            [(Just 3), Nothing, Nothing, Nothing], 
            [(TunedString 7 (Just 3)), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)]
            ),
        (
            "String 2", 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
            [Nothing, (Just 3), Nothing, Nothing], 
            [(TunedString 7 Nothing), (TunedString 0 (Just 3)), (TunedString 4 Nothing), (TunedString 9 Nothing)]
            ),        
        (
            "String 3", 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
            [Nothing, Nothing, (Just 3),  Nothing], 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 (Just 3)), (TunedString 9 Nothing)]
            ),    
        (
            "String 4", 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
            [Nothing, Nothing, Nothing, (Just 3)], 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 (Just 3))]
            ), 
        (
            "All Strings at a time", 
            [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
            [(Just 1), (Just 2), (Just 3), (Just 4)], 
            [(TunedString 7 (Just 1)), (TunedString 0 (Just 2)), (TunedString 4 (Just 3)), (TunedString 9 (Just 4))]
            )                 
        ]

    testApplyFingeringsOnInstrument :: Spec
    testApplyFingeringsOnInstrument = do
        describe "testApplyFingeringsOnInstrument" $
            forM_  
                instrumentFingeringTestCases $
                \(descr, instrument, fingerings, expected) -> 
                    it descr $ do
                        (applyFingeringsOnInstrument instrument fingerings) `shouldBe` expected


    testParseInstrumentFromTunings :: Spec
    testParseInstrumentFromTunings = do
        describe "testParseInstrumentFromTunings" $
            it "Basic case" $ do
                (parseInstrumentFromTunings [0, 7, 14, 21]) 
                `shouldBe` 
                [(TunedString 0 Nothing), (TunedString 7 Nothing), (TunedString 14 Nothing), (TunedString 21 Nothing)]

    testParseFingering :: Spec
    testParseFingering = do
        describe "testParseFingering" $ do
            it "Unstrummed" $ do
                (parseFingering "X") `shouldBe` Nothing
            it "Third fret" $ do
                (parseFingering "3") `shouldBe` Just 3

    testParseFingeringsFromList :: Spec
    testParseFingeringsFromList= do
        describe "testParseFingeringsFromList" $ do
            it "Example 1" $ do
                (parseFingeringsFromList " 10 X 10 12") `shouldBe` [(Just 10), Nothing, (Just 10), (Just 12)]

    testGetMinPosition :: Spec
    testGetMinPosition = do
        describe "testGetMinPosition" $ do
            it "Base case" $ do
                (getMinPosition [(TunedString 7 Nothing), (TunedString 0 (Just 4)), (TunedString 4 (Just 3)), (TunedString 9 (Just 2))]) `shouldBe` 2
            it "Interleaved case" $ do
                (getMinPosition [(TunedString 7 Nothing), (TunedString 0 (Just 5)), (TunedString 4 Nothing), (TunedString 9 (Just 3))]) `shouldBe` 3
            it "Interleaved case reversed" $ do
                (getMinPosition [(TunedString 7 Nothing), (TunedString 0 (Just 3)), (TunedString 4 Nothing), (TunedString 9 (Just 5))]) `shouldBe` 3
            it "With Open Strings" $ do
                (getMinPosition [(TunedString 7 Nothing), (TunedString 0 (Just 3)), (TunedString 4 (Just 0)), (TunedString 9 (Just 5))]) `shouldBe` 3

    testGetMaxPosition :: Spec
    testGetMaxPosition = do
        describe "testGetMinPosition" $ do
            it "Base case" $ do
                (getMaxPosition [(TunedString 7 Nothing), (TunedString 0 (Just 4)), (TunedString 4 (Just 3)), (TunedString 9 (Just 2))]) `shouldBe` 4
            it "Interleaved case" $ do
                (getMaxPosition [(TunedString 7 Nothing), (TunedString 0 (Just 5)), (TunedString 4 Nothing), (TunedString 9 (Just 3))]) `shouldBe` 5
            it "Interleaved case reversed" $ do
                (getMaxPosition [(TunedString 7 Nothing), (TunedString 0 (Just 3)), (TunedString 4 Nothing), (TunedString 9 (Just 7))]) `shouldBe` 7

    calculateFingeringforIntervalTestCases :: [(String, Tuning, Fingering, Tuning, Interval, Fingering)]
    calculateFingeringforIntervalTestCases = [
        ("Major Third tunings/Unison",      0, (Just 5), 4, 0, (Just 1)),
        ("Major Third tunings/Second",      0, (Just 5), 4, 2, (Just 3)),
        ("Major Third tunings/Minor Third", 0, (Just 5), 4, 3, (Just 4)),
        ("Major Third tunings/Major Third", 0, (Just 5), 4, 4, (Just 5)),
        ("Major Third tunings/Fourth",      0, (Just 5), 4, 5, (Just 6)),
        ("Major Third tunings/Fifth",       0, (Just 5), 4, 7, (Just 8)),
        ("Fourths tunings/Unison",          0, (Just 5), 5, 0, (Just 0)),
        ("Fourths tunings/Second",          0, (Just 5), 5, 2, (Just 2)),
        ("Fourths tunings/Minor Third",     0, (Just 5), 5, 3, (Just 3)),
        ("Fourths tunings/Major Third",     0, (Just 5), 5, 4, (Just 4)),
        ("Fourths tunings/Fourth",          0, (Just 5), 5, 5, (Just 5)),
        ("Fourths tunings/Fifth",           0, (Just 5), 5, 7, (Just 7))
        ] 

    testCalculateFingeringforInterval :: Spec
    testCalculateFingeringforInterval = do
        describe "testCalculateFingeringforInterval" $
            forM_  
                calculateFingeringforIntervalTestCases $
                \(descr, baseTuning, baseFingering, stringTuning, interval, expected) -> 
                    it descr $ do
                        (calculateFingeringforInterval baseTuning baseFingering stringTuning interval) `shouldBe` expected


    stringIntervalsTestCases = [
        ("Major Third tunings/Unison", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 0, (TunedString 4 (Just 1))),
        ("Major Third tunings/Second", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 2, (TunedString 4 (Just 3))),
        ("Major Third tunings/Minor Third", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 3, (TunedString 4 (Just 4))),
        ("Major Third tunings/Major Third", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 4, (TunedString 4 (Just 5))),
        ("Major Third tunings/Fourth", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 5, (TunedString 4 (Just 6))),
        ("Major Third tunings/Fifth", (TunedString 0 (Just 5)), (TunedString 4 Nothing), 7, (TunedString 4 (Just 8))),
        ("Fourths tunings/Unison", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 0, (TunedString 5 (Just 0))),
        ("Fourths tunings/Second", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 2, (TunedString 5 (Just 2))),
        ("Fourths tunings/Minor Third", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 3, (TunedString 5 (Just 3))),
        ("Fourths tunings/Major Third", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 4, (TunedString 5 (Just 4))),
        ("Fourths tunings/Fourth", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 5, (TunedString 5 (Just 5))),
        ("Fourths tunings/Fifth", (TunedString 0 (Just 5)), (TunedString 5 Nothing), 7, (TunedString 5 (Just 7)))
        ] 

    testFindFingeringForIntervalOnTunedString :: Spec
    testFindFingeringForIntervalOnTunedString = do
        describe "testFindfingeringForIntervalOnTunedString" $
            forM_  
                stringIntervalsTestCases $
                \(descr, baseNoteTunedString, searchTunedString, interval, expected) -> 
                    it descr $ do
                        (findFingeringForIntervalOnTunedString baseNoteTunedString searchTunedString interval) `shouldBe` expected
