module Main where

import InstrumentModel.Model 
import InstrumentModel.Ukulele
import Rendering.TextRender

renderChordsList :: [(String, Strumento)] -> IO()
renderChordsList [] = putStrLn "*"
renderChordsList ((descr, s) : ss) = do 
        putStrLn $ renderStrumento descr s
        renderChordsList ss

main :: IO ()
main = do 
        renderChordsList chords
        -- putStrLn $ renderStrumento "" uku5
        -- putStrLn $ renderStrumento "" chord1
        -- putStrLn $ renderStrumento "" chord2
        -- putStrLn $ renderStrumento "" chord3
        -- putStrLn $ renderStrumento "" chord4
        -- putStrLn $ renderStrumento "Accordo x" chordx

