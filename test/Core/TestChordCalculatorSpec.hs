module Core.TestChordCalculatorSpec (spec) where

    import Test.Hspec
    import Control.Monad

    import Core.Model
    import Core.ChordCalculator

    spec :: Spec
    spec = do 
        describe "Test module InstrumentModel.Model" $ do
            testApplyFingeringOnTunedString
            testApplyFingeringsOnInstrument

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


 

    instrumentFingeringTestCases :: [(String, Instrument, [Fingering], Instrument)]
    instrumentFingeringTestCases = [
        ("String 1", 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
        [(Just 3), Nothing, Nothing, Nothing], 
        [(TunedString 7 (Just 3)), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)]
        ),
        ("String 2", 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
        [Nothing, (Just 3), Nothing, Nothing], 
        [(TunedString 7 Nothing), (TunedString 0 (Just 3)), (TunedString 4 Nothing), (TunedString 9 Nothing)]
        ),        
        ("String 3", 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
        [Nothing, Nothing, (Just 3),  Nothing], 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 (Just 3)), (TunedString 9 Nothing)]
        ),    
        ("String 4", 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 Nothing)], 
        [Nothing, Nothing, Nothing, (Just 3)], 
        [(TunedString 7 Nothing), (TunedString 0 Nothing), (TunedString 4 Nothing), (TunedString 9 (Just 3))]
        ), 
        ("All Strings at a time", 
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
