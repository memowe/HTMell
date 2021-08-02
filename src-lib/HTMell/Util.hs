-- |Useful utility functions
module HTMell.Util
    ( compose
    , splitNodePath
    , PseudoContent(..)
    , cempty
    ) where

import HTMell.Content ( HTMellContent(..) )
import Data.Char ( isDigit )
import Data.Map ( empty )
import qualified Data.Text as T
import Text.ParserCombinators.ReadP ( ReadP, char, munch1, option, readP_to_S )

-- Handy tools ---------------------------------------------------------

compose :: [a -> a] -> (a -> a)
-- ^ Composition of a list of composable functions
compose = foldr (.) id

-- Parsing ord and path from file/dir name -----------------------------

ordNum :: ReadP Integer
ordNum = do
    num <- read <$> munch1 isDigit
    char '_'
    return num

ordNodePath :: ReadP (Integer, String)
ordNodePath = do
    num <- option 0 ordNum
    rest <- munch1 (/= '.')
    return (num, rest)

splitNodePath :: String -> (Integer, String)
-- ^Splits a given file or directory name in ord integer and path,
--  if it has a leading number. The extension is stripped.
--
-- prop> splitNodePath "42_foo.bar" == (42, "foo")
-- prop> splitNodePath "foo" == (0, "foo")
splitNodePath = fst . head . readP_to_S ordNodePath

-- Pseudo/Empty content instance, useful for testing -------------------

-- | Trivial pseudo content: useful for testing
data PseudoContent = PseudoContent
    deriving (Eq, Show)

instance HTMellContent PseudoContent where
    getContent  = const $ return $ Just PseudoContent
    metadata    = const empty
    toHTML      = const $ T.pack ""

cempty :: Maybe PseudoContent
-- ^ An empty "pseudo" 'HTMell.Content.HTMellContent', useful for testing
cempty = Just PseudoContent
