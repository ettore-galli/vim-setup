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
 

