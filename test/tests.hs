import Test.Tasty ( defaultMain, testGroup )
import HTMell.TestTree ( testTree )
import HTMell.TestContent ( testContent )
import HTMell.TestUtil ( testUtil )

unitTests = testGroup "Unit tests"
    [ testTree
    , testContent
    , testUtil
    ]

main = defaultMain $ testGroup "Tests" [unitTests]
