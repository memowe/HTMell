module Main (main) where

import System.Exit (exitFailure)

import HTMell.TestUtil ( testUtil )

testsPass = and [testUtil]

main :: IO ()
main = if testsPass
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
