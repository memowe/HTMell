module HTMell.Tree
    ( HNode(..)
    , HTree(..)
    , splitNodePath
    ) where

import Data.Char ( isDigit )
import Data.Tree ( Tree )
import Data.Maybe ( fromMaybe )
import Text.ParserCombinators.ReadP ( ReadP, char, munch1, option, readP_to_S )

-- |Representing a node in the generated content tree as something
--  with a path and an ord for sorting. The filepath can belong to
--  a directory (inner node) or a file (content leaf)
data HNode = HNode {
    -- | The path to find this node from its parent
    path :: String,
    -- | defines an order for all children of its parent
    ord :: Integer
} deriving (Eq, Show)

type HTree = Tree HNode

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
