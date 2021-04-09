module Main where

import System.Exit (exitFailure)
import HTMell.Path (HTPath, htpath, htconcat, (</>), htlast, htrel)
import Data.List (intercalate)

sep     = "/"
parts   = ["foo", "bar", "baz"]
pathStr = intercalate sep parts
path    = htpath pathStr

showOK = show path == pathStr

eqOK =  path == htpath "foo/bar/baz"
    &&  path /= htpath "foo/baz"

normalizeOK = htpath "a/../b/c/../../foo/bar/d/../baz" == path

readOK = show parsedPath == pathStr
    where parsedPath = read pathStr :: HTPath

lastOK = htlast path == last parts

concatOK =  show newP1 == show newP2
    &&      show newP2 == expStr
    where
        expStr  = pathStr ++ sep ++ appStr
        appStr  = "quux" ++ sep ++ "quuux"
        path2   = htpath appStr
        newP1   = htconcat path path2
        newP2   = path </> path2

testRel :: String -> String -> String -> Bool
testRel base path expected = relPathStr == expected
    where
        b = htpath base
        p = htpath path
        relPathStr = show $ htrel b p

relOK = testRel ""      "a/b/c"     "a/b/c"
    &&  testRel "a/b/c" ""          ""
    &&  testRel "a/b/c" "a/b/c/d/e" "d/e"
    &&  testRel "a/b/c" "a/b/x/y"   "../x/y"
    &&  testRel "a/b/c" "a/x/y/z"   "../../x/y/z"

pathOK
    =   showOK
    &&  eqOK
    &&  normalizeOK
    &&  readOK
    &&  lastOK
    &&  concatOK
    &&  relOK

main = if pathOK
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
