module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU

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
    assertEqual getPosTasto (Corda 1 0 (Tasto 4)) 4
