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

writeRaw :: IO FilePath
writeRaw = do
    tmpFile <- (</> "42_raw.html") <$> testDirectory
    writeFile tmpFile "<h1>Hello HTMell</h1>"
    return tmpFile

rawIO :: IO (FilePath, Maybe RawHTMLContent)
rawIO = do
    fileName <- writeRaw
    content <- getContent fileName
    return (fileName, content)

cleanupRaw :: (FilePath, Maybe RawHTMLContent) -> IO ()
cleanupRaw = removeFile . fst

testRaw fileContent = testGroup "Raw HTML Content from file"
    [ testCase "Correct metadata" $ do
        content <- fromJust . snd <$> fileContent
        metadata content @?= empty
    , testCase "Correct content" $ do
        content <- fromJust . snd <$> fileContent
        toHTML content @?= T.pack "<h1>Hello HTMell</h1>"
    ]

testRawHTML = withResource rawIO cleanupRaw testRaw

testContent = testGroup "Content tests"
    [ testRawHTML
    ]
