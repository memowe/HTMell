module TestHTMellTree (testHTMellTree) where

import HTMell.Tree (HNode(HNode), parseHNode)

testHNodeParsing = case parseHNode "42_foo.bar" of
    Nothing     -> False
    Just hnode  -> hnode == HNode "foo.bar" 42

testHTMellTree = and [testHNodeParsing]
