module TestHTMellTree ( testHTMellTree ) where

import HTMell.Tree ( splitNodePath )

testHNodeParsingWithOrd = o == 42 && p == "foo.bar"
    where (o, p) = splitNodePath "42_foo.bar"

testHNodeParsingWithoutOrd = o == 0 && p == "foo.bar"
    where (o, p) = splitNodePath "foo.bar"

testHTMellTree = and [
        testHNodeParsingWithOrd,
        testHNodeParsingWithoutOrd
    ]
