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


    getStringKeyPosition :: InstrumentString -> Int
    getStringKeyPosition c = if (tasto c) == X then 0 else extractPos $ tasto c
        where extractPos (Fret t) = t
            


    data InstrumentString = InstrumentString {ordine :: Int
                    , accordatura :: Int
                    , tasto :: Fret
                    } deriving Show

    instance Eq InstrumentString where  
        x == y = (ordine x) == (ordine y)

    type Strumento = [InstrumentString] 

    type Intervallo = Int
    type Accordo = [Intervallo]


    getNoMute :: Strumento -> Strumento
    getNoMute s = (filter (\c -> (tasto c) /= X) s)


    getMinPos :: Strumento -> Int 
    getMinPos s = getMinPosRun $ getNoMute s
        where
            getMinPosRun [] = 0
            getMinPosRun [c] = getStringKeyPosition c 
            getMinPosRun (s:ss) = min (getStringKeyPosition s) (getMinPosRun ss)


    getMaxPos :: Strumento -> Int 
    getMaxPos [] = 0
    getMaxPos [c] = getStringKeyPosition c 
    getMaxPos (s:ss) = max (getStringKeyPosition s) (getMaxPos ss)


    eseguiNota :: Int -> Fret -> Strumento -> Strumento
    eseguiNota _ _ [] = []
    eseguiNota o (Fret t) (u:us) = (if (ordine u) == o then u{tasto=(Fret t)} else u) : (eseguiNota o (Fret t) us)


    cercaDiteggiatureIntervallo :: [InstrumentString] -> InstrumentString -> Intervallo -> [InstrumentString]
    cercaDiteggiatureIntervallo [] _ _ = []
    cercaDiteggiatureIntervallo (s:ss) cref i = (if ((s /= cref) && ((tasto s)) == X && nFret > 0) 
                                                then s{tasto = (Fret nFret)}
                                                else s) : (cercaDiteggiatureIntervallo ss cref i)
        where   accordaturaRef = mod (accordatura cref) 12 -- accordatura corda vuota
                notaRef = mod ((accordatura cref) + (getStringKeyPosition cref)) 12 -- nota risultante base
                notaX = notaRef + i -- nota desiderata
                nFret = mod (notaX - (accordatura s)) 12 -- tasto sulla corda


    cercaDiteggiatureIntervalloAltre :: [InstrumentString] -> InstrumentString -> Intervallo -> [InstrumentString]
    cercaDiteggiatureIntervalloAltre s cref i = cercaDiteggiatureIntervallo 
            (filter (\c -> (tasto c) == X) s)
            cref
            i

    distanzaPosizioni :: InstrumentString -> InstrumentString -> Int
    distanzaPosizioni c d = abs ((getStringKeyPosition c) - (getStringKeyPosition d))


    -- La migliore è la piu vicina
    calcolaMiglioreDiteggiaturaIntervallo :: InstrumentString -> [InstrumentString] -> Maybe InstrumentString
    calcolaMiglioreDiteggiaturaIntervallo cref [] = Nothing 
    calcolaMiglioreDiteggiaturaIntervallo cref (f:fs) = Just (foldl (\acc curr -> if (distanzaPosizioni cref curr) <= (distanzaPosizioni cref acc) then curr else acc ) f (f:fs))


    impostaDiteggiaturaStrumento  :: Strumento -> Maybe InstrumentString -> Strumento
    impostaDiteggiaturaStrumento [] _ = []
    impostaDiteggiaturaStrumento str Nothing = str 
    impostaDiteggiaturaStrumento (s:ss) (Just c) = (if (s==c) then s{tasto=(tasto c)} else s) : (impostaDiteggiaturaStrumento ss (Just c))


    cordaRif :: Strumento -> InstrumentString
    cordaRif [] = InstrumentString 0 0 X
    cordaRif (s:ss) = if (tasto s /= X) then s else (cordaRif ss)


    calcolaDiteggiaturaAccordo :: Strumento -> Accordo -> Strumento 
    calcolaDiteggiaturaAccordo str acc = foldl 
                        (\s i -> 
                            impostaDiteggiaturaStrumento s (
                                calcolaMiglioreDiteggiaturaIntervallo cref (
                                        cercaDiteggiatureIntervalloAltre str cref i
                                    )
                                )
                            ) 
                        str 
                        acc
                    where cref = cordaRif str    

    {-

    C      G      D      A
    0....:....1....:....2..
    CCDDEFFGGAABCCDDEFFGGAABC
    # #  # # #  # #  # # # 

    
    -}