import Test.Tasty ( defaultMain, testGroup )
import HTMell.TestTree ( testTree )
import HTMell.TestTreeLoad ( testTreeLoad )
import HTMell.TestContent ( testContent )
import HTMell.TestUtil ( testUtil )

unitTests = testGroup "Unit tests"
  [ testTree
  , testTreeLoad
  , testContent
  , testUtil
  ]

main = defaultMain $ testGroup "Tests" [unitTests]
