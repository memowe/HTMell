module HTMell.TestFrontmatter ( testFrontmatter ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Content.Metadata.Frontmatter ( readFrontmatter, splitFrontmatter )
import Data.Map ( fromList )

source =  "---\n\
          \foo: bar\n\
          \   baz  :quux   \n\
          \---\n\
          \something else"

expected = fromList
  [ ("baz", "quux")
  , ("foo", "bar")
  ]

testFrontmatter = testGroup "Frontmatter metadata tests"
  [ testCase "Parsing frontmatter" $ readFrontmatter source @?= expected
  , testCase "Splitting frontmatter/rest" $
      splitFrontmatter source @?= (expected, "something else")
  ]
