module Core.ChordCalculator where

    import Data.Maybe
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

    {-|
    Get minimum position (open string excluded)
    -}
    getMinPosition :: Instrument -> Int
    getMinPosition instrument = minimum $ filter (\n -> n > 0) $ map (\(TunedString _ j) -> if isJust j then fromJust j else 0) instrument

    {-|
    Get maximum position
    -}
    getMaxPosition :: Instrument -> Int
    getMaxPosition instrument = maximum $ map (\(TunedString _ j) -> if isJust j then fromJust j else 0) instrument


    calculateFingeringforInterval :: Tuning -> Fingering -> Tuning -> Interval -> Fingering
    calculateFingeringforInterval baseTuning baseFingering stringTuning interval = Just position
        where
            baseNote = baseTuning + fromJust baseFingering
            desiredNote = (baseNote + interval) `mod` 12
            position = (desiredNote - stringTuning) `mod` 12

    {-|
 

    tuning + fingering = baseNote + interval

    -}
    findFingeringForIntervalOnTunedString :: TunedString -> TunedString -> Interval -> TunedString
    findFingeringForIntervalOnTunedString baseNoteTunedString searchTunedString interval  = searchTunedString{
            fingering = calculateFingeringforInterval (tuning baseNoteTunedString) (fingering baseNoteTunedString) (tuning searchTunedString) interval
        }

    {-|
    find a interval on an instrument
    -}
    findIntervalOnInstrument :: Instrument -> Interval -> Instrument
    findIntervalOnInstrument instrument interval = undefined

    {-|
    TODO: ???
    -}
    addIntervalToTuningtry :: Tuning -> Interval -> Tuning
    addIntervalToTuningtry a b = a + b 