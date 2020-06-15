module InstrumentModel.Ukulele where
    import InstrumentModel.Model 

    uku5 :: Strumento
    uku5 = [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
    
    uku35 :: Strumento
    uku35 = [(InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 

    start = eseguiNota 2 (Fret 3) uku5

    chordsSpec = [
        ("2: Maggiore", start, [4, 7]),
        ("2: Minore", start, [3, 7]),
        ("2: Maggiore7", start, [4, 7, 10]),
        ("2: Maggiore7no5", start, [4, 10]),
        ("2: Minore7", start, [3, 7, 10]),
        ("2: Minore7no5", start, [3, 10])
        ]


    chords = map (\ (desc, start, chord) -> (desc, calcolaDiteggiaturaAccordo start chord)) chordsSpec

    chord1 = eseguiNota 1 (Fret 7) uku5
    chord2 = eseguiNota 2 (Fret 6) chord1
    chord3 = eseguiNota 3 (Fret 5) chord2
    chord4 = eseguiNota 4 (Fret 4) chord3
    
    maggiore = [4, 7]
    settima = [4, 10]

    corda = cordaRif start

    ditegg3a = cercaDiteggiatureIntervalloAltre start corda 4
    ditegg3s = calcolaMiglioreDiteggiaturaIntervallo corda ditegg3a
    step1 = impostaDiteggiaturaStrumento start ditegg3s
    
    ditegg7a = cercaDiteggiatureIntervalloAltre step1 corda 10
    ditegg7s = calcolaMiglioreDiteggiaturaIntervallo corda ditegg7a
    step2 = impostaDiteggiaturaStrumento step1 ditegg7s

    chord7 = calcolaDiteggiaturaAccordo start settima

    crif = cordaRif start
 
    da = cercaDiteggiatureIntervalloAltre start crif 4
    das = calcolaMiglioreDiteggiaturaIntervallo crif da
    m1 = impostaDiteggiaturaStrumento start das
    
    db = cercaDiteggiatureIntervalloAltre m1 crif 10
    dbs = calcolaMiglioreDiteggiaturaIntervallo crif db
    m2 = impostaDiteggiaturaStrumento m1 dbs

 
 
         