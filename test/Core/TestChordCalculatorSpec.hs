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

    fingeringTestCases :: [(String, TunedString, Fret, TunedString)]
    fingeringTestCases = [
        ("No fingering", (TunedString 0 X), X, (TunedString 0 X)),
        ("First Fret", (TunedString 0 X), (Fret 1), (TunedString 0 (Fret 1))),
        ("Change Fret", (TunedString 0 (Fret 1)), (Fret 2), (TunedString 0 (Fret 2)))
        ]

    testApplyFingeringOnTunedString :: Spec
    testApplyFingeringOnTunedString = do
        describe "testApplyFingeringOnTunedString" $
            forM_  
                fingeringTestCases $
                \(descr, string, fret, expected) -> 
                    it descr $ do
                        (applyFingeringOnTunedString string fret) `shouldBe` expected


 

    instrumentFingeringTestCases :: [(String, Instrument, [Maybe Fret], Instrument)]
    instrumentFingeringTestCases = [
        ("String 1", 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)], 
        [Just (Fret 3), Nothing, Nothing, Nothing], 
        [(TunedString 7 (Fret 3)), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)]
        ),
        ("String 2", 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)], 
        [Nothing, Just (Fret 3), Nothing, Nothing], 
        [(TunedString 7 X), (TunedString 0 (Fret 3)), (TunedString 4 X), (TunedString 9 X)]
        ),        
        ("String 3", 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)], 
        [Nothing, Nothing, Just (Fret 3),  Nothing], 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 (Fret 3)), (TunedString 9 X)]
        ),    
        ("String 4", 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)], 
        [Nothing, Nothing, Nothing, Just (Fret 3)], 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 (Fret 3))]
        ), 
        ("All Strings at a time", 
        [(TunedString 7 X), (TunedString 0 X), (TunedString 4 X), (TunedString 9 X)], 
        [Just (Fret 1), Just (Fret 2), Just (Fret 3), Just (Fret 4)], 
        [(TunedString 7 (Fret 1)), (TunedString 0 (Fret 2)), (TunedString 4 (Fret 3)), (TunedString 9 (Fret 4))]
        )                 
        ]

    testApplyFingeringsOnInstrument :: Spec
    testApplyFingeringsOnInstrument = do
        describe "testApplyFingeringsOnInstrument" $
            forM_  
                instrumentFingeringTestCases $
                \(descr, instrument, frets, expected) -> 
                    it descr $ do
                        (applyFingeringsOnInstrument instrument frets) `shouldBe` expected
