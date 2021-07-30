module HTMell.Test.Util
    ( someDigits
    , testDirectory
    ) where

import System.Random ( newStdGen, Random(randomRs) )
import System.Directory ( getTemporaryDirectory, createDirectory )
import System.FilePath ( (</>) )

someDigits :: Int -> IO String
someDigits n = take n . randomRs ('0','9') <$> newStdGen

testDirectory :: IO FilePath
testDirectory = do
    system <- getTemporaryDirectory
    digits <- someDigits 6
    let dirName = system </> "HTMell-Test-" ++ digits
    createDirectory dirName
    return dirName
