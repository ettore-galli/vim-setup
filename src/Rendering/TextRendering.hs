
module Rendering.TextRendering where

    import Core.Model 
    import Data.Maybe

    newLine :: String
    newLine = "\r\n"

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
    renderMuteOrSeparator (Just n) = "  "

    renderSingleString :: Fingering -> Int -> Int -> String
    renderSingleString fingering minp maxp = 
        (renderMuteOrSeparator fingering) ++ 
        concat (
            map 
            (\p -> if (isJust fingering) && ((fromJust fingering) == p) then "|-o-" else "|---") 
            [minp..maxp]
        ) 


    {-
    renderInstrumentStringTastata:: Int -> Int -> InstrumentString -> String
    renderInstrumentStringTastata minp mp c = if mp >= minp 
                                    then 
                                        (renderInstrumentStringTastata minp (mp - 1) c) 
                                        ++ (if (mp == getFretPosition c) then "|-o-" else "|---")
                                    else " "    


    renderInstrumentStringMuta:: Int -> Int -> String
    renderInstrumentStringMuta minp mp = if mp >= minp 
                                    then 
                                        (renderInstrumentStringMuta minp (mp - 1)) ++  "|---"
                                    else "X"  


    renderInstrumentString :: Int -> Int -> InstrumentString -> String
    renderInstrumentString minp maxp c = (show $ stringNumber c) ++ ": " ++ 
        if (fingeredFret c) == X then renderInstrumentStringMuta minp maxp 
        else renderInstrumentStringTastata minp maxp c
    


    

    renderFrettedInstrument :: String -> FrettedInstrument -> String
    renderFrettedInstrument _ [] = ""
    renderFrettedInstrument descr s = descr ++ "  " ++ (short s) ++ newLine ++
                                "     " ++ (renderCapo minp) ++ newLine ++ 
                                renderRun minp maxp s
        where minp = getMinPos s
                maxp = minp + 5   
                renderRun :: Int -> Int -> FrettedInstrument -> String
                renderRun _ _ [] = ""
                renderRun minp maxp (s:ss) = (renderRun minp maxp ss) ++ (renderInstrumentString minp maxp s) ++ newLine

    short :: FrettedInstrument -> String
    short [] = ""
    short (s:ss) = (if (fingeredFret s == X) then "X" else (show $ getFretPosition s)) ++ "." ++ (short ss)
    -}