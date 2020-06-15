module InstrumentModel.Ukulele where
    import InstrumentModel.Model 

    uku5 :: FrettedInstrument
    uku5 = [(InstrumentString 1 0 X), (InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 
    
    uku35 :: FrettedInstrument
    uku35 = [(InstrumentString 2 7 X), (InstrumentString 3 14 X), (InstrumentString 4 21 X)] 

    start = fingerNote 2 (Fret 3) uku5

    chordsSpec = [
        ("2: Maggiore", start, [4, 7]),
        ("2: Minore", start, [3, 7]),
        ("2: Maggiore7", start, [4, 7, 10]),
        ("2: Maggiore7no5", start, [4, 10]),
        ("2: Minore7", start, [3, 7, 10]),
        ("2: Minore7no5", start, [3, 10])
        ]


    chords = map (\ (desc, start, chord) -> (desc, getFingeringForChordDefinition start chord)) chordsSpec

    chord1 = fingerNote 1 (Fret 7) uku5
    chord2 = fingerNote 2 (Fret 6) chord1
    chord3 = fingerNote 3 (Fret 5) chord2
    chord4 = fingerNote 4 (Fret 4) chord3
    
    maggiore = [4, 7]
    settima = [4, 10]

    corda = getChordBaseString start

    ditegg3a = findChromaticIntervalFingeringsOnOtherStrings start corda 4
    ditegg3s = calculateBestFingeringForChromaticInterval corda ditegg3a
    step1 = setFingeringOnFrettedInstrument start ditegg3s
    
    ditegg7a = findChromaticIntervalFingeringsOnOtherStrings step1 corda 10
    ditegg7s = calculateBestFingeringForChromaticInterval corda ditegg7a
    step2 = setFingeringOnFrettedInstrument step1 ditegg7s

    chord7 = getFingeringForChordDefinition start settima

    crif = getChordBaseString start
 
    da = findChromaticIntervalFingeringsOnOtherStrings start crif 4
    das = calculateBestFingeringForChromaticInterval crif da
    m1 = setFingeringOnFrettedInstrument start das
    
    db = findChromaticIntervalFingeringsOnOtherStrings m1 crif 10
    dbs = calculateBestFingeringForChromaticInterval crif db
    m2 = setFingeringOnFrettedInstrument m1 dbs

 
 
         