module HTMell.TestMarkdown ( testMarkdown ) where

import Test.Tasty ( testGroup, TestTree, withResource )
import Test.Tasty.HUnit ( (@?=), testCase )
import HTMell.Test.Util ( testDirectory )
import System.Directory ( removeFile )
import System.FilePath ( (</>) )
import HTMell.Content ( HTMellContent(..) )
import HTMell.Content.Markdown ( readMarkdown, MarkdownContent(..) )
import Data.Map ( fromList )
import Data.Maybe ( fromJust )
import qualified Data.Text as T

source =  "---\n\
          \foo: bar\n\
          \---\n\
          \# Hello HTMell!\n"

testMarkdownExtraction = testGroup "Content extraction"
  [ testCase "Correct metadata" $
      metadata (readMarkdown source) @?= fromList [("foo", "bar")]
  , testCase "Correct HTML" $
      toHTML (readMarkdown source)
        @?= T.pack "<h1>Hello HTMell!</h1>\n"
  ]

testMarkdownLoading = withResource io cleanup test
  where
    test mdIO = testCase "Filesystem loading" $ do
      mc <- fromJust . snd <$> mdIO
      mc @?= readMarkdown source
    io = do
      name    <- write
      content <- getContent name
      return (name, content)
    write = do
      file <- (</> "HTMell-Markdown-Test.md") <$> testDirectory
      writeFile file source
      return file
    cleanup = removeFile . fst

testMarkdown = testGroup "Markdown tests"
  [ testMarkdownExtraction
  , testMarkdownLoading
  ]
