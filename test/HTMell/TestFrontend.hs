module HTMell.TestFrontend ( testFrontend ) where

import Test.Tasty ( testGroup, withResource )
import Test.Tasty.HUnit ( (@?=), testCase )
import HTMell.Test.Util ( createFile, testDirectory )
import HTMell ( loadHTMell, get, getHTML )
import HTMell.Tree ( HTree(), content, summary )
import HTMell.Content ( HTMellContent(..) )
import HTMell.Content.Markdown ( MarkdownContent(..) )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Directory ( removeDirectoryRecursive )

testFrontend = withResource io cleanup testLoadedFrontend
  where io      = do  dir     <- write
                      htmell  <- loadHTMell dir
                      return (dir, htmell)
        write   = do  dir <- testDirectory
                      createFile (dir </> "foo.md") "# Foo"
                      return dir
        cleanup = removeDirectoryRecursive . fst

testLoadedFrontend dirIO = testGroup "HTMell frontend tests"
  [ testCase "Correct structure loaded" $ do
      tree <- fromJust . snd <$> dirIO
      summary tree @?= "(foo)"
  , testCase "Find correct node via get" $ do
      tree <- fromJust . snd <$> dirIO
      let node = fromJust $ get tree "foo"
      let html = toHTML $ fromJust $ content node
      html @?= T.pack "<h1>Foo</h1>\n"
  , testCase "Get correct HTML via getHTML" $ do
      tree <- fromJust . snd <$> dirIO
      fromJust (getHTML tree "foo") @?= T.pack "<h1>Foo</h1>\n"
  ]
