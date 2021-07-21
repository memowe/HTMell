module HTMell.TestTree ( testTree ) where

import HTMell.Tree ( HTree(..), summary, findHNode )
import Data.Map ( empty, fromList )
import Data.Maybe ( isNothing, fromJust )

trivialTree = HTree 42 empty
childTree   = HTree 17 (fromList [("foo", HTree 42 empty)])
exampleTree = HTree 17 (fromList [
        ("foo", HTree 42 (fromList [
            ("bar", HTree 37 (fromList [
                ("baz", HTree 108 empty),
                ("quux", HTree 109 empty)
            ])),
            ("bidu", HTree 25 empty)
        ])),
        ("xnorfzt", HTree 666 empty)
    ])

testSummary
    =   summary trivialTree == ""
    &&  summary childTree == "(foo)"
    &&  summary exampleTree == "(foo(bar(baz,quux),bidu),xnorfzt)"

testFindHNode
    =   isNothing (findHNode trivialTree "foo")
    &&  isNothing (findHNode childTree "")
    &&  findHNode childTree "foo" == Just (HTree 42 empty)
    &&  summary (fromJust (findHNode exampleTree "foo/bar")) == "(baz,quux)"
    &&  summary (fromJust (findHNode exampleTree "foo/bar/quux")) == ""

testTree
    =   testSummary
    &&  testFindHNode
