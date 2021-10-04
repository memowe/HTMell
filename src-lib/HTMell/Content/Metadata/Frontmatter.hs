{-|
Module      : HTMell.Content.Metadata.Frontmatter
Description : Oversimplified frontmatter parser
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

A parser for over-simplified frontmatter metadata
-}

module HTMell.Content.Metadata.Frontmatter
  (
  -- * Over-simplified frontmatter
  -- $NotFrontmatter

  -- * Parsing from strings
    readFrontmatter
  , splitFrontmatter
  , parseFrontmatter
  ) where

import Data.Map ( Map, fromList, empty )
import Data.List ( dropWhile, dropWhileEnd )
import Text.ParserCombinators.ReadP
  ( char, many, munch, munch1, readP_to_S, skipMany1, string, ReadP )

{- $NotFrontmatter
Frontmatter usually means a section of YAML data at the beginning of
text-based files (like markdown data), surrounded by @"---"@:

@
---
front: matter
answer: 42
color: blue
---
Rest of the document
...
@

This parser only understands an oversimplified version, with simple
key-value pairs only. For most use cases this should be enough though.
-}

trimC x       = dropWhile (==x) . dropWhileEnd (==x)
trim          = trimC ' '
newlines      = skipMany1 (char '\n')
without chars = munch1 (`notElem` chars)

separator :: ReadP ()
separator = do { string "---"; munch (== '-'); newlines; return () }

pair :: ReadP (String, String)
pair = do key <- trim <$> without "\n:"
          char ':'
          value <- trim <$> without "\n"
          newlines
          return (key, value)

frontmatter :: ReadP (Map String String)
frontmatter = do  separator
                  pairs <- many pair
                  separator
                  return $ fromList pairs

-- | Extracts a 'Map' of frontmatter key-value pairs from the given 'String'
readFrontmatter :: String -> Map String String
readFrontmatter = fst . splitFrontmatter

-- | Splits the given 'String' in a pair of frontmatter data and the rest,
-- without separators.
splitFrontmatter :: String -> (Map String String, String)
splitFrontmatter md | null parses = (empty, md)
                    | otherwise   = last parses
  where parses = parseFrontmatter md

-- | The 'Map' parser, useful for debugging.
parseFrontmatter :: ReadS (Map String String)
parseFrontmatter = readP_to_S frontmatter
