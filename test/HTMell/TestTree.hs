module HTMell.TestTree ( testTree ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
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

testSummary = testGroup "Tree summary"
    [ testCase "Empty tree" $ summary trivialTree @?= ""
    , testCase "Single child" $ summary childTree @?= "(foo)"
    , testCase "Complex tree" $
        summary exampleTree @?= "(foo(bar(baz,quux),bidu),xnorfzt)"
    ]

-- Helper operator for simplified summary testing of HTrees
a @?=| b = summary (fromJust a) @?= b

testFindHNode = testGroup "Find HNodes"
    [ testCase "Empty tree" $
        findHNode trivialTree "foo" @?= Nothing
    , testCase "Empty query" $
        findHNode childTree "" @?= Nothing
    , testCase "Direct child" $
        findHNode childTree "foo" @?= Just (HTree 42 empty)
    , testCase "Complex subtree query" $
        findHNode exampleTree "foo/bar" @?=| "(baz,quux)"
    , testCase "Complex leaf query" $
        findHNode exampleTree "foo/bar/quux" @?=| ""
    ]

testTree = testGroup "Content tree tests"
    [ testSummary
    , testFindHNode
    ]
