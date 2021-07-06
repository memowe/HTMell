module TestHTMellTree ( testHTMellTree ) where

import HTMell.Tree ( parseNodeOrd )

testHNodeParsing = case parseNodeOrd "42_foo.bar" of
    Nothing     -> False
    Just (o, p) -> o == 42 && p == "foo.bar"

testHTMellTree = and [testHNodeParsing]
