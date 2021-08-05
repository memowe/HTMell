{-|
Module      : HTMell.Content.Markdown
Description : HTMell markdown content
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

Loading HTMell content from markdown files
-}

module HTMell.Content.Markdown
  (
  -- * What is markdown content?
    MarkdownContent(..)
  -- * Obtaining markdown content
  , readMarkdown
  , loadMarkdownContent
  ) where

import HTMell.Content ( HTMellContent(..) )
import HTMell.Content.Metadata.Frontmatter ( splitFrontmatter )
import Data.Map ( Map )
import qualified Data.Text as T
import Data.Text ( Text )
import CMark ( commonmarkToHtml )

-- | A markdown content value consists of metadata and markdown data. The
-- 'metadata' is read as "HTMell.Content.Metadata.Frontmatter", the
-- markdown is just the source after the frontmatter data and will be
-- translated to HTML via "CMark" when calling 'toHTML'.
data MarkdownContent = MarkdownContent
  { meta :: Map String String
  , markdown :: String
  } deriving (Eq, Show)

instance HTMellContent MarkdownContent where
  getContent  = loadMarkdownContent
  metadata    = meta
  toHTML      = commonmarkToHtml [] . T.pack . markdown

-- | Build a 'MarkdownContent' value from a source 'String'.
readMarkdown :: String -> MarkdownContent
readMarkdown = uncurry MarkdownContent . splitFrontmatter

-- | Read a 'MarkdownContent' value from a file.
loadMarkdownContent :: FilePath -> IO (Maybe MarkdownContent)
loadMarkdownContent path = Just . readMarkdown <$> readFile path
