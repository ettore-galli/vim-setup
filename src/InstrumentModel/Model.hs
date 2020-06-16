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


    getInstrumentStringKeyPosition :: InstrumentString -> Int
    getInstrumentStringKeyPosition c = if (fingeredFret c) == X then 0 else extractFretPosition $ fingeredFret c
        where extractFretPosition (Fret t) = t
            


    data InstrumentString = InstrumentString {stringNumber :: Int
                    , stringTuning :: Int
                    , fingeredFret :: Fret
                    } deriving Show

    instance Eq InstrumentString where  
        x == y = (stringNumber x) == (stringNumber y)

    type FrettedInstrument = [InstrumentString] 

    type ChromaticInterval = Int
    type ChordDefinition = [ChromaticInterval]


    getNoMute :: FrettedInstrument -> FrettedInstrument
    getNoMute s = (filter (\c -> (fingeredFret c) /= X) s)


    getMinPos :: FrettedInstrument -> Int 
    getMinPos s = getMinPosRun $ getNoMute s
        where
            getMinPosRun [] = 0
            getMinPosRun [c] = getInstrumentStringKeyPosition c 
            getMinPosRun (s:ss) = min (getInstrumentStringKeyPosition s) (getMinPosRun ss)


    getMaxPos :: FrettedInstrument -> Int 
    getMaxPos [] = 0
    getMaxPos [c] = getInstrumentStringKeyPosition c 
    getMaxPos (s:ss) = max (getInstrumentStringKeyPosition s) (getMaxPos ss)


    fingerNote :: Int -> Fret -> FrettedInstrument -> FrettedInstrument
    fingerNote _ _ [] = []
    fingerNote strNum (Fret t) (u:us) = (if (stringNumber u) == strNum then u{fingeredFret=(Fret t)} else u) : (fingerNote strNum (Fret t) us)


    findPossibleFingeringsForChromaticInterval :: [InstrumentString] -> InstrumentString -> ChromaticInterval -> [InstrumentString]
    findPossibleFingeringsForChromaticInterval [] _ _ = []
    findPossibleFingeringsForChromaticInterval (s:ss) cref i = (if ((s /= cref) && ((fingeredFret s)) == X && nFret > 0) 
                                                then s{fingeredFret = (Fret nFret)}
                                                else s) : (findPossibleFingeringsForChromaticInterval ss cref i)
        where   stringTuningRef = mod (stringTuning cref) 12 -- stringTuning corda vuota
                notaRef = mod ((stringTuning cref) + (getInstrumentStringKeyPosition cref)) 12 -- nota risultante base
                notaX = notaRef + i -- nota desiderata
                nFret = mod (notaX - (stringTuning s)) 12 -- fingeredFret sulla corda

    -- TODO: Serve?
    findChromaticIntervalFingeringsOnOtherStrings :: [InstrumentString] -> InstrumentString -> ChromaticInterval -> [InstrumentString]
    findChromaticIntervalFingeringsOnOtherStrings s cref i = findPossibleFingeringsForChromaticInterval 
            (filter (\c -> (fingeredFret c) == X) s)
            cref
            i

    getFingeringsDistance :: InstrumentString -> InstrumentString -> Int
    getFingeringsDistance c d = abs ((getInstrumentStringKeyPosition c) - (getInstrumentStringKeyPosition d))


    calculateBestFingeringForChromaticInterval :: InstrumentString -> [InstrumentString] -> Maybe InstrumentString
    calculateBestFingeringForChromaticInterval cref [] = Nothing 
    calculateBestFingeringForChromaticInterval cref (f:fs) = Just (foldl (\acc curr -> if (getFingeringsDistance cref curr) <= (getFingeringsDistance cref acc) then curr else acc ) f (f:fs))


    setFingeringOnFrettedInstrument  :: FrettedInstrument -> Maybe InstrumentString -> FrettedInstrument
    setFingeringOnFrettedInstrument [] _ = []
    setFingeringOnFrettedInstrument str Nothing = str 
    setFingeringOnFrettedInstrument (s:ss) (Just c) = (if (s==c) then s{fingeredFret=(fingeredFret c)} else s) : (setFingeringOnFrettedInstrument ss (Just c))


    getChordBaseString :: FrettedInstrument -> InstrumentString
    getChordBaseString [] = InstrumentString 0 0 X
    getChordBaseString (s:ss) = if (fingeredFret s /= X) then s else (getChordBaseString ss)


    getFingeringForChordDefinition :: FrettedInstrument -> ChordDefinition -> FrettedInstrument 
    getFingeringForChordDefinition str acc = foldl 
                        (\s i -> 
                            setFingeringOnFrettedInstrument s (
                                calculateBestFingeringForChromaticInterval cref (
                                        findChromaticIntervalFingeringsOnOtherStrings str cref i
                                    )
                                )
                            ) 
                        str 
                        acc
                    where cref = getChordBaseString str    

    {-

    C      G      D      A
    0....:....1....:....2..
    CCDDEFFGGAABCCDDEFFGGAABC
    # #  # # #  # #  # # # 

    
    -}