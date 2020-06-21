module InstrumentModel.Model where
     
    data Fret = X | Fret Int deriving Show 

    instance Eq Fret where  
        X  == X = True
        X  == _ = False
        _  == X = False
        (Fret x) == (Fret y) = x == y  

    instance Ord Fret where  
        compare X X = EQ
        compare X _ = LT
        compare _ X = GT
        compare (Fret x) (Fret y) = compare x y


    getFretPosition :: InstrumentString -> Int
    getFretPosition c = if (fingeredFret c) == X then 0 else extractFretPosition $ fingeredFret c
        where extractFretPosition (Fret t) = t
            


    data InstrumentString = InstrumentString {stringNumber :: Int
                    , stringTuning :: Int
                    , fingeredFret :: Fret
                    } deriving (Show, Eq)

    sameInstrumentString :: InstrumentString -> InstrumentString -> Bool
    sameInstrumentString x y = (stringNumber x) == (stringNumber y)

    type FrettedInstrument = [InstrumentString] 

    type ChromaticInterval = Int
    type ChordDefinition = [ChromaticInterval]


    getNoMute :: FrettedInstrument -> FrettedInstrument
    getNoMute s = (filter (\c -> (fingeredFret c) /= X) s)


    getMinPos :: FrettedInstrument -> Int 
    getMinPos s = getMinPosRun $ getNoMute s
        where
            getMinPosRun [] = 0
            getMinPosRun [c] = getFretPosition c 
            getMinPosRun (s:ss) = min (getFretPosition s) (getMinPosRun ss)


    getMaxPos :: FrettedInstrument -> Int 
    getMaxPos [] = 0
    getMaxPos [c] = getFretPosition c 
    getMaxPos (s:ss) = max (getFretPosition s) (getMaxPos ss)


    fingerNote :: Int -> Fret -> FrettedInstrument -> FrettedInstrument
    fingerNote _ _ [] = []
    fingerNote strNum (Fret t) (u:us) = (if (stringNumber u) == strNum then u{fingeredFret=(Fret t)} else u) : (fingerNote strNum (Fret t) us)


    findPossibleFingeringsForChromaticInterval :: [InstrumentString] -> InstrumentString -> ChromaticInterval -> [InstrumentString]
    findPossibleFingeringsForChromaticInterval [] _ _ = []
    findPossibleFingeringsForChromaticInterval (s:ss) refInstrumentString i = (
                                                if ((not $ sameInstrumentString s refInstrumentString) && ((fingeredFret s) == X) && (desiredNoteFretNumber > 0)) 
                                                then s{fingeredFret = (Fret desiredNoteFretNumber)}
                                                else s) : (findPossibleFingeringsForChromaticInterval ss refInstrumentString i)
        where   refStringTuning = mod (stringTuning refInstrumentString) 12 -- This is the tuning for the unfretted string
                refStringNote = mod ((stringTuning refInstrumentString) + (getFretPosition refInstrumentString)) 12 -- This is the actual note of the reference string given the fret presse
                desiredNote = refStringNote + i -- This is the desired note (interval from the reerence actual note)
                desiredNoteFretNumber = mod (desiredNote - (stringTuning s)) 12 -- This is the fret number for the desired note, on the actual ("new" string)


    findFingeringOnString :: InstrumentString -> InstrumentString -> ChromaticInterval -> InstrumentString
    findFingeringOnString st ref i = st{fingeredFret = (Fret desiredNoteFretNumber)}
        where   refStringTuning = mod (stringTuning ref) 12 -- This is the tuning for the unfretted string
                refStringNote = mod ((stringTuning ref) + (getFretPosition ref)) 12 -- This is the actual note of the reference string given the fret presse
                desiredNote = refStringNote + i -- This is the desired note (interval from the reerence actual note)
                desiredNoteFretNumber = mod (desiredNote - (stringTuning st)) 12 -- This is the fret number for the desired note, on the actual ("new" string)

    {-
    findPossibleFingeringsForChromaticIntervalNew :: [InstrumentString] -> InstrumentString -> ChromaticInterval -> [InstrumentString]
    findPossibleFingeringsForChromaticIntervalNew [] _ _ = []
    findPossibleFingeringsForChromaticIntervalNew strgs refInstrumentString interval = 
        map  (\s -> findFingeringOnString s refInstrumentString interval) strgs
        -- $
        --    filter (\s -> ((fingeredFret s) == X) && (not $ sameInstrumentString s refInstrumentString)) strgs
        
--        (
--                                                if ((not $ sameInstrumentString s refInstrumentString) && ((fingeredFret s) == X) && (desiredNoteFretNumber > 0)) 
--                                                then s{fingeredFret = (Fret desiredNoteFretNumber)}
--                                                else s) : (findPossibleFingeringsForChromaticIntervalNew ss refInstrumentString i)
--        where   refStringTuning = mod (stringTuning refInstrumentString) 12 -- This is the tuning for the unfretted string
--                refStringNote = mod ((stringTuning refInstrumentString) + (getFretPosition refInstrumentString)) 12 -- This is the actual note of the reference string given the fret presse
--                desiredNote = refStringNote + i -- This is the desired note (interval from the reerence actual note)
--                desiredNoteFretNumber = mod (desiredNote - (stringTuning s)) 12 -- This is the fret number for the desired note, on the actual ("new" string)
-}

    findChromaticIntervalFingeringsOnOtherStrings :: [InstrumentString] -> InstrumentString -> ChromaticInterval -> [InstrumentString]
    findChromaticIntervalFingeringsOnOtherStrings s refInstrumentString i = findPossibleFingeringsForChromaticInterval 
            (filter (\c -> (fingeredFret c) == X) s)
            refInstrumentString
            i

    getFingeringsDistance :: InstrumentString -> InstrumentString -> Int
    getFingeringsDistance c d = abs ((getFretPosition c) - (getFretPosition d))


    calculateBestFingeringForChromaticInterval :: InstrumentString -> [InstrumentString] -> Maybe InstrumentString
    calculateBestFingeringForChromaticInterval refInstrumentString [] = Nothing 
    calculateBestFingeringForChromaticInterval refInstrumentString (f:fs) = Just (foldl (\acc curr -> if (getFingeringsDistance refInstrumentString curr) <= (getFingeringsDistance refInstrumentString acc) then curr else acc ) f (f:fs))


    setFingeringOnFrettedInstrument  :: FrettedInstrument -> Maybe InstrumentString -> FrettedInstrument
    setFingeringOnFrettedInstrument [] _ = []
    setFingeringOnFrettedInstrument str Nothing = str 
    setFingeringOnFrettedInstrument (s:ss) (Just c) = (if (sameInstrumentString s c) then s{fingeredFret=(fingeredFret c)} else s) : (setFingeringOnFrettedInstrument ss (Just c))


    getChordBaseString :: FrettedInstrument -> InstrumentString
    getChordBaseString [] = InstrumentString 0 0 X
    getChordBaseString (s:ss) = if (fingeredFret s /= X) then s else (getChordBaseString ss)


    getFingeringForChordDefinition :: FrettedInstrument -> ChordDefinition -> FrettedInstrument 
    getFingeringForChordDefinition str acc = foldl 
                        (\s i -> 
                            setFingeringOnFrettedInstrument s (
                                calculateBestFingeringForChromaticInterval refInstrumentString (
                                        -- findChromaticIntervalFingeringsOnOtherStrings str refInstrumentString i
                                        findPossibleFingeringsForChromaticInterval str refInstrumentString i
                                    )
                                )
                            ) 
                        str 
                        acc
                    where refInstrumentString = getChordBaseString str    

    {-

    C      G      D      A
    0....:....1....:....2..
    CCDDEFFGGAABCCDDEFFGGAABC
    # #  # # #  # #  # # # 

    
    -}