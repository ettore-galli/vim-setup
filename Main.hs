module Main where

import InstrumentModel.Model 
import InstrumentModel.Ukulele
import Rendering.TextRender

main :: IO ()
main = do 
        putStrLn $ renderStrumento uku5
        putStrLn $ renderStrumento chord1
        putStrLn $ renderStrumento chord2
        putStrLn $ renderStrumento chord3
        putStrLn $ renderStrumento chord4
        putStrLn $ renderStrumento chordx

