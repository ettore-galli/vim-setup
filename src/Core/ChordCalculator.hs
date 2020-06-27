module Core.ChordCalculator where

    import Core.Model

    {-|
    Given a string and a fret, apply the fingering to the string.
    -}
    applyFingeringOnTunedString :: TunedString -> Fingering -> TunedString
    applyFingeringOnTunedString t Nothing = t
    applyFingeringOnTunedString (TunedString tuning _ ) fingering = TunedString tuning fingering

    {-|
    Given an instrument and and a corresponding list of frets, apply the fingering(s) to the instrument.
    -}
    applyFingeringsOnInstrument :: Instrument -> [Fingering] -> Instrument
    applyFingeringsOnInstrument [] _ = []
    applyFingeringsOnInstrument _ [] = []
    applyFingeringsOnInstrument (s:ss) (f:fs) = s : (applyFingeringsOnInstrument ss fs)

    {-|
    Given an instrument, string and fingering build list of frets for fingerings
    -}    
    

    findIntervalOnTunedString :: TunedString -> Interval -> TunedString
    findIntervalOnTunedString string interval = undefined

    findIntervalOnInstrument :: Instrument -> Interval -> Instrument
    findIntervalOnInstrument instrument interval = undefined