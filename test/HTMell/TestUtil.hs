module HTMell.TestUtil ( testUtil ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Util ( splitNodePath, pathParts )

testNodePathParsing = testGroup "Node path parsing"
  [ testCase "With ord number" $
      splitNodePath "42_foo.bar" @?= (42, "foo")
  , testCase "Without ord number" $
      splitNodePath "foo.bar" @?= (0, "foo")
  , testCase "Without extension" $
      splitNodePath "17_foo" @?= (17, "foo")
  ]

testPathPartSplitting = testGroup "Path part splitting"
  [ testCase "Empty path" $
      pathParts "" @?= []
  , testCase "Simple path" $
      pathParts "foo/bar" @?= words "foo bar"
  , testCase "Complex path" $
      pathParts "//foo/bar/baz/" @?= words "foo bar baz"
  ]

testUtil = testGroup "Utility tests"
  [ testNodePathParsing
  , testPathPartSplitting
  ]
