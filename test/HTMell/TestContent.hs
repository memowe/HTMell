module HTMell.TestContent ( testContent ) where

import Test.Tasty ( testGroup, withResource, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Test.Util ( testDirectory )
import System.Directory ( removeFile )
import System.FilePath ((</>))
import HTMell.Content ( RawHTMLContent(..), HTMellContent (getContent, toHTML, metadata) )
import Data.Maybe ( fromJust )
import Data.Map ( empty )
import qualified Data.Text as T
import Data.Text ( pack )

testRawHTML fileContent = testGroup "Raw HTML Content from file"
    [ testCase "Correct metadata" $ do
        content <- fromJust . snd <$> fileContent
        metadata content @?= empty
    , testCase "Correct content" $ do
        content <- fromJust . snd <$> fileContent
        toHTML content @?= T.pack "<h1>Hello HTMell</h1>"
    ]

testRawHTMLFile = withResource io cleanup testRawHTML
    where
        io :: IO (FilePath, Maybe RawHTMLContent)
        io = do
            name <- write
            content <- getContent name
            return (name, content)
        write :: IO FilePath
        write = do
            file <- (</> "42_raw.html") <$> testDirectory
            writeFile file "<h1>Hello HTMell</h1>"
            return file
        cleanup :: (FilePath, Maybe RawHTMLContent) -> IO ()
        cleanup = removeFile . fst

testContent = testGroup "Content tests"
    [ testRawHTMLFile
    ]
