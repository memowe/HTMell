module TestHTMellTree ( testHTMellTree ) where

import HTMell.Tree ( splitNodePath )

testHNodeParsingWithOrd = case splitNodePath "42_foo.bar" of
    Nothing     -> False
    Just (o, p) -> o == 42 && p == "foo.bar"

testHNodeParsingWithoutOrd = case splitNodePath "foo.bar" of
    Nothing     -> False
    Just (o, p) -> o == 0 && p == "foo.bar"

testHTMellTree = and [
        testHNodeParsingWithOrd,
        testHNodeParsingWithoutOrd
    ]
