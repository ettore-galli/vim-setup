module Demo.Showcase where

    import Control.Monad

    import Core.Model
    import Core.KnownInstruments
    import Core.ChordCalculator

    import Rendering.TextRendering
    


    showcases = [
        ("G", (applyFingeringsOnInstrument ukuleleHighG [Nothing, Just 2, Just 3, Just 2])),
        ("Gm", (applyFingeringsOnInstrument ukuleleHighG [Nothing, Just 2, Just 3, Just 1])),
        ("C", (applyFingeringsOnInstrument ukuleleHighG [Nothing, Just 0, Just 0, Just 3]))
        ]

    demo = do 
            putStrLn "DEMO"
            forM_  showcases (\(descr, instrument) ->
                    putStrLn $ renderFingering descr instrument
                )
