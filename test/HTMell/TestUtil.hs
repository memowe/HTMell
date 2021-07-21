module HTMell.TestUtil ( testUtil ) where

import HTMell.Util ( splitNodePath )

testHNodeParsingWithOrd = o == 42 && p == "foo.bar"
    where (o, p) = splitNodePath "42_foo.bar"

testHNodeParsingWithoutOrd = o == 0 && p == "foo.bar"
    where (o, p) = splitNodePath "foo.bar"

testUtil
    =   testHNodeParsingWithOrd
    &&  testHNodeParsingWithoutOrd
