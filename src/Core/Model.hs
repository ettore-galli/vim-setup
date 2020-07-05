module Core.Model where
     
    {-| 
    Single fret fingering, Nothing for unstrummed string, 0 for open string 
    -}
    type Fingering = Maybe Int  

    {-| 
    String relative tuning in semitones
    -}
    type Tuning = Int  

    {-| 
    Note in semitones
    -}
    type Note = Int  

    {-| 
    A single string, the way it is tuned and how it is fingered.
    tuning: The tuning expressed in arbitrary relative semitones 
            (e.g. the lowest tuned string might be 0)
    fret: The fingering being applied to the string; X if the string is not played        
    -}
    data TunedString = TunedString {
        tuning :: Tuning, 
        fingering :: Fingering
        } deriving (Show, Eq)

    {-|
    An instrument, i.e. an ordered list of tuned strings, fingered or not
    -}
    type Instrument = [TunedString]    


    {-|
    An interval, expressed in semitones (e.g 4 is a major third)
    -}
    type Interval = Int


    {-|
    A chord, described as a list of desired intervals. 
    
    Note that the order has no connection to the order of the strings that
    constitute an instrument; nor it is guaranteed that all intervals will be found
    on the instrument keyboard.
    
    The order should represent the priority of the interval seeked, the first having
    higher priority.
    -}
    type Chord = [Interval]