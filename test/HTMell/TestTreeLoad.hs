module HTMell.TestTreeLoad ( testTreeLoad ) where

import Test.Tasty ( withResource, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Test.Util ( testDirectory, createFile )
import HTMell.Tree ( HNode(..), summary, findHNode )
import HTMell.Tree.Load ( buildTree )
import HTMell.Content ( HTMellContent(..), RawHTMLContent(..) )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Directory ( removeDirectoryRecursive )

testTreeLoad = withResource io cleanup testLoadedTree
    where
        io :: IO (FilePath, Maybe (HNode RawHTMLContent))
        io = do
            dir <- write
            tree <- buildTree dir
            return (dir, tree)
        write :: IO FilePath
        write = do
            dir <- testDirectory
            createFile (dir </> "2_foo.html")                   "<h1>Foo</h1>"
            createFile (dir </> "3_bar.html")                   "<h1>Bar</h1>"
            createFile (dir </> "1_quux" </> "42_answer.html")  "<h1>42</h1>"
            createFile (dir </> "1_quux" </> "17_17.html")      "<h1>17</h1>"
            createFile (dir </> "1_quux" </> "index.html")      "<h1>Quux</h1>"
            return dir
        cleanup :: (FilePath, Maybe (HNode RawHTMLContent)) -> IO ()
        cleanup = removeDirectoryRecursive . fst

conTest :: HNode RawHTMLContent -> String -> String
conTest tree path = T.unpack html
    where
        node = fromJust $ findHNode tree path
        html = toHTML $ fromJust $ content node

testLoadedTree :: IO (FilePath, Maybe (HNode RawHTMLContent)) -> TestTree
testLoadedTree dirIO = testGroup "Content tree loading tests"
    [ testCase "Correct structure" $ do
        tree <- fromJust . snd <$> dirIO
        summary tree @?= "(quux(17,answer),foo,bar)"
    , testCase "Correct /foo content" $ do
        tree <- fromJust . snd <$> dirIO
        conTest tree "/foo" @?= "<h1>Foo</h1>"
    , testCase "Correct /quux/answer content" $ do
        tree <- fromJust . snd <$> dirIO
        conTest tree "/quux/answer" @?= "<h1>42</h1>"
    ]
