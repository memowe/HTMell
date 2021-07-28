module HTMell.TestContent ( testContent ) where

import Test.Tasty ( testGroup, withResource, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import System.Directory ( getTemporaryDirectory, removeFile )
import HTMell.Content ( RawHTMLContent(..), HTMellContent (getContent, toHTML, metadata) )
import Data.Maybe ( fromJust )
import Data.Map ( empty )
import qualified Data.Text as T
import Data.Text ( pack )

prepareRawContent :: IO (FilePath, Maybe RawHTMLContent)
prepareRawContent = do
    tmpDir <- getTemporaryDirectory
    let tmpFile = tmpDir ++ "/" ++ "42_raw.html"
    writeFile tmpFile "<h1>Hello HTMell</h1>"
    content <- getContent tmpFile
    return (tmpFile, content)

cleanupRawContent :: (FilePath, Maybe RawHTMLContent) -> IO ()
cleanupRawContent = removeFile . fst

testRawContent fileContent = testGroup "Raw HTML Content from file"
    [ testCase "Correct metadata" $ do
        content <- fromJust . snd <$> fileContent
        metadata content @?= empty
    , testCase "Correct content" $ do
        content <- fromJust . snd <$> fileContent
        toHTML content @?= T.pack "<h1>Hello HTMell</h1>"
    ]

testRawHTMLContent = withResource
    prepareRawContent
    cleanupRawContent
    testRawContent

testContent = testGroup "Content tests"
    [ testRawHTMLContent
    ]
