module Main where

import InstrumentModel.Model 
import InstrumentModel.Ukulele
import Rendering.TextRender

main :: IO ()
main = defaultMain allTests 

allTests :: TestTree
allTests = testGroup "Chords Tests" [
    testGroup "MOdel function Tests" [ testGetPosTasto ]
    ]


testGetPosTasto :: TestTree
testGetPosTasto = HU.testCase "Test get Key Position" $ 
    assertEqual "aaa" 1 1
