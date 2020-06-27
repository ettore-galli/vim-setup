module Main where

import Legacy.InstrumentModel.Model 
import Legacy.InstrumentModel.Ukulele
import Legacy.Rendering.TextRender

renderChordsList :: [(String, FrettedInstrument)] -> IO()
renderChordsList [] = putStrLn "*"
renderChordsList ((descr, s) : ss) = do 
        putStrLn $ renderFrettedInstrument descr s
        renderChordsList ss

main :: IO ()
main = do 
        renderChordsList chords
 

