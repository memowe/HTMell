module Main (main) where

import System.Exit (exitFailure)

import HTMell.TestTree ( testTree )
import HTMell.TestUtil ( testUtil )

testsPass
    =   testTree
    &&  testUtil

main :: IO ()
main = if testsPass
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
