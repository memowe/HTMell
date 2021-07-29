module HTMell.TestTree ( testTree ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Tree ( HNode(..), childList, summary, findHNode )
import HTMell.Util ( cempty )
import Data.Map ( empty, (!), fromList )
import Data.Maybe ( isNothing, fromJust )

trivialTree = HNode 42 empty cempty
childTree   = HNode 17 (fromList [("foo", HNode 42 empty cempty)]) cempty
exampleTree = HNode 17 (fromList [
        ("foo", HNode 42 (fromList [
            ("bar", HNode 37 (fromList [
                ("baz", HNode 108 empty cempty),
                ("quux", HNode 109 empty cempty)
            ]) cempty),
            ("bidu", HNode 25 empty cempty)
        ]) cempty),
        ("xnorfzt", HNode 666 empty cempty)
    ]) cempty

testSummary = testGroup "Tree summary"
    [ testCase "Empty tree" $ summary trivialTree @?= ""
    , testCase "Single child" $ summary childTree @?= "(foo)"
    , testCase "Complex tree" $
        summary exampleTree @?= "(foo(bidu,bar(baz,quux)),xnorfzt)"
    ]

testChildList = testGroup "Sorted list of children"
    [ testCase "Empty tree" $ childList trivialTree @?= []
    , testCase "Single child" $
        childList childTree @?= [("foo", children childTree ! "foo")]
    , testCase "Complex tree 'foo' children" $
        childList foo @?= map (\p -> (p, children foo ! p)) ["bidu", "bar"]
    , testCase "Complex tree 'foo/bar' children" $
        childList bar @?= map (\p -> (p, children bar ! p)) ["baz", "quux"]
    ]
    where
        foo = children exampleTree ! "foo"
        bar = children foo ! "bar"

-- Helper operator for simplified summary testing of HNodes
a @?=| b = summary (fromJust a) @?= b

testFindHNode = testGroup "Find HNodes"
    [ testCase "Empty tree" $
        findHNode trivialTree "foo" @?= Nothing
    , testCase "Empty query" $
        findHNode childTree "" @?= Nothing
    , testCase "Direct child" $
        findHNode childTree "foo" @?= Just (HNode 42 empty cempty)
    , testCase "Complex subtree query" $
        findHNode exampleTree "foo/bar" @?=| "(baz,quux)"
    , testCase "Complex leaf query" $
        findHNode exampleTree "foo/bar/quux" @?=| ""
    ]

testTree = testGroup "Content tree tests"
    [ testSummary
    , testChildList
    , testFindHNode
    ]
