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
    splitNodePath
  -- * Content
  , PseudoContent(..)
  , cempty
  -- * Other stuff
  , compose
  ) where

import HTMell.Content ( HTMellContent(..) )
import Data.Char ( isDigit )
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

-- Parsing ord and path from file/dir name -----------------------------

ordNum :: ReadP Integer
ordNum = do
  num <- read <$> munch1 isDigit
  char '_'
  return num

ordNodePath :: ReadP (Integer, String)
ordNodePath = do
  num   <- option 0 ordNum
  rest  <- munch1 (/= '.')
  return (num, rest)

-- | Splits a given file or directory name in 'ord' 'Integer' and name, if
-- it has a leading number, followed by @"_"@. The extension is stripped.
--
-- prop> splitNodePath "42_foo.bar" == (42, "foo")
-- prop> splitNodePath "foo" == (0, "foo")
splitNodePath :: String -> (Integer, String)
splitNodePath = fst . head . readP_to_S ordNodePath

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
