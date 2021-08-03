module HTMell.Test.Util
    ( someDigits
    , testDirectory
    , createFile
    ) where

import System.Random ( newStdGen, Random(randomRs) )
import System.Directory ( getTemporaryDirectory, createDirectoryIfMissing )
import System.FilePath ( (</>), takeDirectory )

someDigits :: Int -> IO String
someDigits n = take n . randomRs ('0','9') <$> newStdGen

testDirectory :: IO FilePath
testDirectory = do
    system <- getTemporaryDirectory
    digits <- someDigits 6
    let dirName = system </> "HTMell-Test-" ++ digits
    createDirectoryIfMissing True dirName
    return dirName

createFile :: FilePath -> String -> IO ()
createFile path content = do
    createDirectoryIfMissing True dirName
    writeFile path content
    where dirName = takeDirectory path
