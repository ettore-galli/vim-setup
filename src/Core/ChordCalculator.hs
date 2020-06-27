module Core.ChordCalculator where

    import Core.Model

    {-|
    Given a string and a fret, apply the fingering to the string.
    -}
    applyFingeringOnTunedString :: TunedString -> Fret -> TunedString
    applyFingeringOnTunedString (TunedString tuning _ ) fret = TunedString tuning fret

    {-|
    Given an instrument and and a corresponding list of frets, apply the fingering(s) to the instrument.
    -}
    applyFingeringsOnInstrument :: Instrument -> [Maybe Fret] -> Instrument
    applyFingeringsOnInstrument [] _ = []
    applyFingeringsOnInstrument _ [] = []
    applyFingeringsOnInstrument (s:ss) ((Nothing):fs) = s : (applyFingeringsOnInstrument ss fs)
    applyFingeringsOnInstrument (s:ss) ((Just fret):fs) = (applyFingeringOnTunedString s fret) : (applyFingeringsOnInstrument ss fs)


    findIntervalOnTunedString :: TunedString -> Interval -> TunedString
    findIntervalOnTunedString string interval = undefined

    findIntervalOnInstrument :: Instrument -> Interval -> Instrument
    findIntervalOnInstrument instrument interval = undefined