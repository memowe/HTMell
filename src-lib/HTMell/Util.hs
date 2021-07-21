-- |Useful utility functions
module HTMell.Util
    ( splitNodePath
    ) where

import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP ( ReadP, char, munch1, option, readP_to_S )

-- Parsing ord and path from file/dir name -------------------------------------

ordNum :: ReadP Integer
ordNum = do
    num <- read <$> munch1 isDigit
    char '_'
    return num

ordNodePath :: ReadP (Integer, String)
ordNodePath = do
    num <- option 0 ordNum
    rest <- munch1 (const True)
    return (num, rest)

splitNodePath :: String -> (Integer, String)
-- ^Splits a given file or directory name in ord integer and path,
--  if it has a leading number.
--
-- prop> splitNodePath "42_foo" == (42, "foo")
-- prop> splitNodePath "foo" == (0, "foo")
splitNodePath = fst . head . readP_to_S ordNodePath
