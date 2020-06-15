module Main where

import InstrumentModel.Model 
import InstrumentModel.Ukulele
import Rendering.TextRender

renderChordsList :: [(String, FrettedInstrument)] -> IO()
renderChordsList [] = putStrLn "*"
renderChordsList ((descr, s) : ss) = do 
        putStrLn $ renderFrettedInstrument descr s
        renderChordsList ss

main :: IO ()
main = do 
        renderChordsList chords
 

