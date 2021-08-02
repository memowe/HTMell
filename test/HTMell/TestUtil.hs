module HTMell.TestUtil ( testUtil ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Util ( splitNodePath )

testNodePathParsing = testGroup "Node path parsing"
    [ testCase "With ord number" $
        splitNodePath "42_foo.bar" @?= (42, "foo")
    , testCase "Without ord number" $
        splitNodePath "foo.bar" @?= (0, "foo")
    , testCase "Without extension" $
        splitNodePath "17_foo" @?= (17, "foo")
    ]

testUtil = testGroup "Utility tests"
    [ testNodePathParsing
    ]
