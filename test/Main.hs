module Main (main) where

import System.Exit (exitFailure)

import TestHTMellTree (testHTMellTree)

testsPass = and [testHTMellTree]

main :: IO ()
main = if testsPass
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
