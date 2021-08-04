import Test.Tasty ( defaultMain, testGroup )
import HTMell.TestTree ( testTree )
import HTMell.TestTreeLoad ( testTreeLoad )
import HTMell.TestContent ( testContent )
import HTMell.TestFrontmatter ( testFrontmatter )
import HTMell.TestFrontend ( testFrontend )
import HTMell.TestUtil ( testUtil )

unitTests = testGroup "Unit tests"
  [ testTree
  , testTreeLoad
  , testContent
  , testFrontmatter
  , testFrontend
  , testUtil
  ]

main = defaultMain $ testGroup "Tests" [unitTests]
