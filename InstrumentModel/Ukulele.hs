module InstrumentModel.Ukulele where
    import InstrumentModel.Model 

    uku5 :: Strumento
    uku5 = [(Corda 1 0 X), (Corda 2 7 X), (Corda 3 14 X), (Corda 4 21 X)] 
    
    chords = [
        ("2: Maggiore", [4, 7]),
        ("2: Minore", [3, 7]),
        ("2: Maggiore7", [4, 7, 10]),
        ("2: Maggiore7no5", [4, 10]),
        ("2: Minore7", [3, 7, 10]),
        ("2: Minore7no5", [3, 10])
        ]

    chord1 = eseguiNota 1 (Tasto 7) uku5
    chord2 = eseguiNota 2 (Tasto 6) chord1
    chord3 = eseguiNota 3 (Tasto 5) chord2
    chord4 = eseguiNota 4 (Tasto 4) chord3
    
    maggiore = [4, 7]
    start = eseguiNota 2 (Tasto 3) uku5
    corda = (start!!1)
    chordx = calcolaDiteggiaturaAccordo start corda maggiore

    terza = calcolaMiglioreDiteggiaturaIntervallo corda (
        cercaDiteggiatureIntervalloAltre start corda 4
        )   