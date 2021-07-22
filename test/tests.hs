import Test.Tasty ( defaultMain, testGroup )
import HTMell.TestTree ( testTree )
import HTMell.TestUtil ( testUtil )

unitTests = testGroup "Unit tests"
    [ testTree
    , testUtil
    ]

main = defaultMain $ testGroup "Tests" [unitTests]
