module HTMell.Content.Markdown
  (
    MarkdownContent(..)
  , readMarkdown
  , loadMarkdownContent
  ) where

import HTMell.Content ( HTMellContent(..) )
import HTMell.Content.Metadata.Frontmatter ( splitFrontmatter )
import Data.Map ( Map )
import qualified Data.Text as T
import Data.Text ( Text )
import CMark ( commonmarkToHtml )

data MarkdownContent = MarkdownContent
  { meta :: Map String String
  , html :: Text
  } deriving (Eq, Show)

instance HTMellContent MarkdownContent where
  getContent  = loadMarkdownContent
  metadata    = meta
  toHTML      = html

readMarkdown :: String -> MarkdownContent
readMarkdown str = MarkdownContent fm html
  where
    (fm, rest)  = splitFrontmatter str
    html        = commonmarkToHtml [] $ T.pack rest

loadMarkdownContent :: FilePath -> IO (Maybe MarkdownContent)
loadMarkdownContent path = Just . readMarkdown <$> readFile path
