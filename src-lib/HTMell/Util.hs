{-|
Module      : HTMell.Util
Description : Useful utility functions
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

Useful utility functions.
-}

module HTMell.Util
  (
  -- * Content trees
    pathParts
  , splitNodePath
  -- * Content
  , PseudoContent(..)
  , cempty
  -- * Other stuff
  , compose
  ) where

import HTMell.Content ( HTMellContent(..) )
import Data.Char ( isDigit )
import Data.List.Split ( splitOn )
import Data.Map ( empty )
import qualified Data.Text as T
import Text.ParserCombinators.ReadP ( ReadP, char, munch1, option, readP_to_S )

-- Handy tools ---------------------------------------------------------

-- | Composes a list of composable functions:
--
-- \[
--  [f_1,f_2,...,f_n] \;\mapsto\; f_1 \circ f_2 \circ \cdots \circ f_n
--  \qquad\mbox{for}\qquad
--  f_i: a \to a \; \forall\;i \,;\;\; a\mbox{ any type.}
-- \]
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- Path part handling

-- | Splits a given content address string in its single path parts.
--
-- prop> pathParts "/foo/bar/baz" == ["foo", "bar", "baz"]
pathParts :: String -> [String]
pathParts = filter (not . null) . splitOn "/"

-- | Splits a given file or directory name in 'HTMell.Tree.ord' 'Integer'
-- and name, if it has a leading number, followed by @"_"@. The extension
-- is stripped.
--
-- prop> splitNodePath "42_foo.bar" == (42, "foo")
-- prop> splitNodePath "foo" == (0, "foo")
splitNodePath :: String -> (Integer, String)
splitNodePath = fst . head . readP_to_S ordNodePath
  where ordNodePath = do  num   <- option 0 ordNum
                          rest  <- munch1 (/= '.')
                          return (num, rest)
        ordNum      = do  num   <- read <$> munch1 isDigit
                          char  '_'
                          return num

-- Pseudo/Empty content instance, useful for testing -------------------

-- | Trivial pseudo content, useful for testing only.
data PseudoContent = PseudoContent
  deriving (Eq, Show)

instance HTMellContent PseudoContent where
  getContent  = const $ return $ Just PseudoContent
  metadata    = const empty
  toHTML      = const $ T.pack ""

-- | An empty 'PseudoContent' value, useful for testing only.
cempty :: Maybe PseudoContent
cempty = Just PseudoContent
