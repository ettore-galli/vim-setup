module InstrumentModel.Model where
     
    data Tasto = X | Tasto Int deriving Show 

    instance Eq Tasto where  
        X  == X = True
        X  == _ = False
        _  == X = False
        (Tasto x) == (Tasto y) = x == y  

    instance Ord Tasto where  
        compare X X = EQ
        compare X _ = LT
        compare _ X = GT
        compare (Tasto x) (Tasto y) = compare x y


    getPosTasto :: Corda -> Int
    getPosTasto c = if (tasto c) == X then 0 else extractPos $ tasto c
        where extractPos (Tasto t) = t
            


    data Corda = Corda {ordine :: Int
                    , accordatura :: Int
                    , tasto :: Tasto
                    } deriving Show

    instance Eq Corda where  
        x == y = (ordine x) == (ordine y)

    type Strumento = [Corda] 

    type Intervallo = Int
    type Accordo = [Intervallo]


    getNoMute :: Strumento -> Strumento
    getNoMute s = (filter (\c -> (tasto c) /= X) s)


    getMinPos :: Strumento -> Int 
    getMinPos s = getMinPosRun $ getNoMute s
        where
            getMinPosRun [] = 0
            getMinPosRun [c] = getPosTasto c 
            getMinPosRun (s:ss) = min (getPosTasto s) (getMinPosRun ss)


    getMaxPos :: Strumento -> Int 
    getMaxPos [] = 0
    getMaxPos [c] = getPosTasto c 
    getMaxPos (s:ss) = max (getPosTasto s) (getMaxPos ss)


    eseguiNota :: Int -> Tasto -> Strumento -> Strumento
    eseguiNota _ _ [] = []
    eseguiNota o (Tasto t) (u:us) = (if (ordine u) == o then u{tasto=(Tasto t)} else u) : (eseguiNota o (Tasto t) us)


    cercaDiteggiatureIntervallo :: [Corda] -> Corda -> Intervallo -> [Corda]
    cercaDiteggiatureIntervallo [] _ _ = []
    cercaDiteggiatureIntervallo (s:ss) cref i = (if ((s /= cref) && ((tasto s)) == X) 
                                                then s{tasto = (Tasto nTasto)}
                                                else s) : (cercaDiteggiatureIntervallo ss cref i)
        where   accordaturaRef = mod (accordatura cref) 12 -- accordatura corda vuota
                notaRef = mod ((accordatura cref) + (getPosTasto cref)) 12 -- nota risultante base
                notaX = notaRef + i -- nota desiderata
                nTasto = mod (notaX - (accordatura s)) 12 -- tasto sulla corda


    cercaDiteggiatureIntervalloAltre :: [Corda] -> Corda -> Intervallo -> [Corda]
    cercaDiteggiatureIntervalloAltre s cref i = cercaDiteggiatureIntervallo 
            (filter (\c -> (tasto c) == X) s)
            cref
            i

    distanzaPosizioni :: Corda -> Corda -> Int
    distanzaPosizioni c d = abs ((getPosTasto c) - (getPosTasto d))


    -- La migliore è la piu vicina
    calcolaMiglioreDiteggiaturaIntervallo :: Corda -> [Corda] -> Maybe Corda
    calcolaMiglioreDiteggiaturaIntervallo cref [] = Nothing 
    calcolaMiglioreDiteggiaturaIntervallo cref (f:fs) = Just (foldl (\acc curr -> if (distanzaPosizioni cref curr) <= (distanzaPosizioni cref acc) then curr else acc ) f (f:fs))


    impostaDiteggiaturaStrumento  :: Strumento -> Maybe Corda -> Strumento
    impostaDiteggiaturaStrumento [] _ = []
    impostaDiteggiaturaStrumento s Nothing = s 
    impostaDiteggiaturaStrumento (s:ss) (Just c) = (if (s==c) then s{tasto=(tasto c)} else s) : (impostaDiteggiaturaStrumento ss (Just c))


    cordaRif :: Strumento -> Corda
    cordaRif [] = Corda 0 0 X
    cordaRif (s:ss) = if (tasto s /= X) then s else (cordaRif ss)


    calcolaDiteggiaturaAccordo :: Strumento -> Accordo -> Strumento
    calcolaDiteggiaturaAccordo s [] = s
    calcolaDiteggiaturaAccordo s (a:as) = calcolaDiteggiaturaAccordo 
                (
                    impostaDiteggiaturaStrumento s (
                    calcolaMiglioreDiteggiaturaIntervallo cref (
                            cercaDiteggiatureIntervalloAltre s cref a
                        )
                    )
                )
                as 
            where cref = cordaRif s
        
     


    {-

    C      G      D      A
    0....:....1....:....2..
    CCDDEFFGGAABCCDDEFFGGAABC
    # #  # # #  # #  # # # 

    
    -}