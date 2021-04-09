module Main where

import System.Exit (exitFailure)
import HTMell.Path (HTPath, htpath, htconcat, (</>), htlast, htrel)
import Data.List (intercalate)

showOK = show (htpath "foo/bar/baz") == "foo/bar/baz"

eqOK =  htpath "foo/bar/baz" == htpath "foo/bar/baz"
    &&  htpath "foo/bar/baz" /= htpath "foo/baz"

normalizeOK = htpath "a/../b/c/../../x/y/d/../z" == htpath "x/y/z"

readOK = show parsedPath == "foo/bar/baz"
    where parsedPath = read "foo/bar/baz" :: HTPath

lastOK = htlast (htpath "foo/bar/baz") == "baz"

concatOK =  show newP1 == show newP2
    &&      show newP2 == "foo/bar/baz/quux/quuux"
    where
        path1 = htpath "foo/bar/baz/xnulch"
        path2 = htpath "../quux/quuux"
        newP1 = htconcat path1 path2
        newP2 = path1 </> path2

testRel :: String -> String -> String -> Bool
testRel base path exp = rel == exp && b </> e == p
    where
        b = htpath base
        p = htpath path
        e = htpath exp
        rel = show $ htrel b p

relOK = testRel ""      "a/b/c"     "a/b/c"
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
