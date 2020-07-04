
module Rendering.TextRendering where

    import Core.Model 
    import Core.ChordCalculator
    import Data.Maybe

    newLine :: String
    newLine = "\r\n"

    separator :: String
    separator = "  "

    {-|
    Render the capo
    -}
    renderCapoName :: Int -> String
    renderCapoName cp  
        | cp == 0 = ""
        | cp == 1 = (show cp) ++ "st"
        | cp == 2 = (show cp) ++ "nd"
        | cp == 3 = (show cp) ++ "rd"
        | otherwise = (show cp) ++ "th"

    renderMuteOrSeparator :: Fingering -> String
    renderMuteOrSeparator Nothing  = "X "
    renderMuteOrSeparator (Just 0) = "0 "
    renderMuteOrSeparator (Just n) = separator

    renderSingleString :: Fingering -> Int -> Int -> String
    renderSingleString fingering minp maxp = 
        (renderMuteOrSeparator fingering) ++ 
        concat (
            map 
            (\p -> if (isJust fingering) && ((fromJust fingering) == p) then "|-o-" else "|---") 
            [minp..maxp]
        ) 

    renderInstrumentFingering :: Instrument -> Int -> Int -> String
    renderInstrumentFingering [] _ _ = ""
    renderInstrumentFingering (s : ss) minp maxp =
         (renderInstrumentFingering ss minp maxp) ++ "\n" ++ (renderSingleString (fingering s) minp maxp) 


    renderChord :: String -> Instrument -> String
    renderChord label instrument = separator ++ label ++  "\n" ++
        separator ++ (renderCapoName minp) ++  
        (renderInstrumentFingering instrument minp maxp) 
        where minp = getMinPosition instrument
              maxp = max (getMaxPosition instrument) (minp + 4)

    