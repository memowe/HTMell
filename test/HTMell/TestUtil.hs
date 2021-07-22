module HTMell.TestUtil ( testUtil ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Util ( splitNodePath )

testNodePathParsing = testGroup "Node path parsing"
    [ testCase "With ord number" $
        splitNodePath "42_foo.bar" @?= (42, "foo.bar")
    , testCase "Wothout ord number" $
        splitNodePath "foo.bar" @?= (0, "foo.bar")
    ]

testUtil = testGroup "Utility tests"
    [ testNodePathParsing
    ]
