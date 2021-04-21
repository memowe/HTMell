module Main where

import System.Exit (exitFailure)
import HTMell.Path (htpath, htpshow, htpeq, htpconcat, (</>), htprel)
import Data.List (intercalate)

constructOK = htpath "foo/bar/baz" == ["foo", "bar", "baz"]

showOK = htpshow (htpath "foo/bar/baz") == "foo/bar/baz"

eqOK =  htpath "foo/bar/baz" `htpeq` htpath "foo/bar/baz"
    &&  not (htpath "foo/bar/baz" `htpeq` htpath "foo/baz")

normalizeOK = htpath "a/../b/c/../../x/y/d/../z" == htpath "x/y/z"

lastOK = last (htpath "foo/bar/baz") == "baz"

concatOK =  htpshow newP1 == htpshow newP2
    &&      htpshow newP2 == "foo/bar/baz/quux/quuux"
    where
        path1 = htpath "foo/bar/baz/xnulch"
        path2 = htpath "../quux/quuux"
        newP1 = htpconcat path1 path2
        newP2 = path1 </> path2

testRel :: String -> String -> String -> Bool
testRel base path exp = rel == exp && b </> e == p
    where
        b = htpath base
        p = htpath path
        e = htpath exp
        rel = htpshow $ htprel b p

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
