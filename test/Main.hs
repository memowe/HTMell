module Main where

import System.Exit (exitFailure)
import HTMell.Path (htpath, htshow, hteq, htconcat, (</>), htrel)
import Data.List (intercalate)

constructOK = htpath "foo/bar/baz" == ["foo", "bar", "baz"]

showOK = htshow (htpath "foo/bar/baz") == "foo/bar/baz"

eqOK =  htpath "foo/bar/baz" `hteq` htpath "foo/bar/baz"
    &&  not (htpath "foo/bar/baz" `hteq` htpath "foo/baz")

normalizeOK = htpath "a/../b/c/../../x/y/d/../z" == htpath "x/y/z"

lastOK = last (htpath "foo/bar/baz") == "baz"

concatOK =  htshow newP1 == htshow newP2
    &&      htshow newP2 == "foo/bar/baz/quux/quuux"
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
        rel = htshow $ htrel b p

relOK = testRel ""      "a/b/c"     "a/b/c"
    &&  testRel "a/b/c" "a/b/c/d/e" "d/e"
    &&  testRel "a/b/c" "a/b/x/y"   "../x/y"
    &&  testRel "a/b/c" "a/x/y/z"   "../../x/y/z"

pathOK
    =   constructOK
    &&  showOK
    &&  eqOK
    &&  normalizeOK
    &&  lastOK
    &&  concatOK
    &&  relOK

main = if pathOK
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
