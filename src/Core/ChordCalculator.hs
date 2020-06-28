module Core.ChordCalculator where

    import Core.Model
    import Core.KnownInstruments

    {-|
    Given a string and a fret, apply the fingering to the string.
    -}
    applyFingeringOnTunedString :: TunedString -- ^ The string on which to apply the fingering
                                -> Fingering   -- ^ The finfering to apply
                                -> TunedString -- ^ The resulting string, with fingering applied
    applyFingeringOnTunedString t Nothing = t
    applyFingeringOnTunedString (TunedString tuning fg) (Just n) = TunedString tuning (Just n) 


    {-|
    Given an instrument, string number and fret build the set of fingerings 
    to apply to a full instrument, i.e. a list of Nothing(s) but the fingering
    received in input.
    -} 
    prepareFingerings :: Instrument     -- ^ The template instrument
                        -> Int          -- ^ The string number
                        -> Fingering    -- ^ The fingering to apply
                        -> [Fingering]  -- ^ The list of fingerings, i.e. no fingerings (Nothing) except for the string specified
    prepareFingerings instrument strn fingering = prepareFingeringsRun instrument strn fingering 1
        where    
            prepareFingeringsRun :: Instrument -> Int -> Fingering -> Int -> [Fingering]
            prepareFingeringsRun [] _ _ _ = [] 
            prepareFingeringsRun (s:ss) strn fingering progr = (if strn == progr then fingering else Nothing) : (prepareFingeringsRun ss strn fingering (progr + 1))

    {-|
    Given an instrument, string number and fret and and a corresponding list of frets, apply the fingering(s) to the instrument.
    -}
    applyFingeringsOnInstrument :: Instrument   -- ^ The instrument on which to apply the fingering
                                -> [Fingering]  -- ^ The list of fingerings to apply
                                -> Instrument   -- ^ The instrument, with fingerings applied
    applyFingeringsOnInstrument [] _ = []
    applyFingeringsOnInstrument _ [] = []
    applyFingeringsOnInstrument (s:ss) (f:fs) = (applyFingeringOnTunedString s f) : (applyFingeringsOnInstrument ss fs)

    {-|
    Given a list of tunings, instantiate an instrument
    -}
    parseInstrumentFromTunings :: [Tuning]    -- ^ List of desired tunings
                                -> Instrument -- ^ The resulting instrument
    parseInstrumentFromTunings tunings = map (\t -> TunedString t Nothing) tunings

    {-|
    Turn a descriptive fingering into a fingering
    -}
    parseFingering :: String -> Fingering
    parseFingering tuning = case reads tuning of
        [] -> Nothing
        [(n, _)] -> Just n

    {-|
    Fingerings from a list of strings or numbers

    Example:
        X 12 0 10 -> First string unstrummed
                     Second string twelfth fret
                     Third string open
                     Fourth string tenth fret   
                     Disclaimer: Don't play this chord on a ukulele, it's horrible...

    -}
    parseFingeringsFromList :: String -> [Fingering]
    parseFingeringsFromList fingerings = map (\t -> parseFingering t) (words fingerings)

    getMinPosition :: Instrument -> Int
    getMinPosition [(TunedString _ (Just n))] = n
    getMinPosition ((TunedString _ Nothing):ss) = getMinPosition ss
    getMinPosition ((TunedString _ (Just n)):ss) = min n (getMinPosition ss) 

    findIntervalOnTunedString :: TunedString -> Interval -> TunedString
    findIntervalOnTunedString string interval = undefined

    findIntervalOnInstrument :: Instrument -> Interval -> Instrument
    findIntervalOnInstrument instrument interval = undefined

    addIntervalToTuningtry :: Tuning -> Interval -> Tuning
    addIntervalToTuningtry a b = a + b 