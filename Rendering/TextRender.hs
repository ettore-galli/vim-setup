
module Rendering.TextRender where

import InstrumentModel.Model 

newLine :: String
newLine = "\r\n"

renderCordaTastata:: Int -> Int -> Corda -> String
renderCordaTastata minp mp c = if mp >= minp 
                                then 
                                    (renderCordaTastata minp (mp - 1) c) 
                                    ++ (if (mp == getPosTasto c) then "|-o-" else "|---")
                                else " "    

renderCordaMuta:: Int -> Int -> String
renderCordaMuta minp mp = if mp >= minp 
                                then 
                                    (renderCordaMuta minp (mp - 1)) ++  "|---"
                                else "X"  

renderCorda :: Int -> Int -> Corda -> String
renderCorda minp maxp c = (show $ ordine c) ++ ": " ++ 
    if (tasto c) == X then renderCordaMuta minp maxp 
    else renderCordaTastata minp maxp c
 
renderCapo :: Int -> String
renderCapo cp  
    | cp == 0 = ""
    | cp == 1 = (show cp) ++ "st"
    | cp == 2 = (show cp) ++ "nd"
    | cp == 3 = (show cp) ++ "rd"
    | otherwise = (show cp) ++ "th"
   

renderStrumento :: String -> Strumento -> String
renderStrumento _ [] = ""
renderStrumento descr s = descr ++ newLine ++
                            "     " ++ (renderCapo minp) ++ newLine ++ 
                            renderRun minp maxp s
      where minp = getMinPos s
            maxp = minp + 3 --getMaxPos s  
            renderRun :: Int -> Int -> Strumento -> String
            renderRun _ _ [] = ""
            renderRun minp maxp (s:ss) = (renderRun minp maxp ss) ++ (renderCorda minp maxp s) ++ newLine