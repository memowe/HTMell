-- | HTMell content related types and functions
module HTMell.Content
  ( HTMellContent(..)
  , RawHTMLContent(..)
  ) where

import System.FilePath ( FilePath, takeExtension )
import Data.Map ( Map, empty )
import Data.Text ( Text )
import Prelude hiding ( readFile )
import Data.Text.IO ( readFile )

-- | Some content stored in 'HTMell.Tree.HNode' trees
class HTMellContent c where
  -- | Extract a content representation
  getContent :: FilePath -> IO (Maybe c)
  -- | Some simple metadata
  metadata :: c -> Map String String
  -- | Translate our content to plain HTML 'Text'
  toHTML :: c -> Text

-- | Content wrapper for raw HTML files, without metadata
newtype RawHTMLContent = RawHTMLContent Text deriving (Eq, Show)

isRawHTML :: FilePath -> Bool
isRawHTML = (== ".html") . takeExtension

instance HTMellContent RawHTMLContent where
  getContent fp
    | isRawHTML fp  = Just . RawHTMLContent <$> readFile fp
    | otherwise     = return Nothing
  metadata = const empty
  toHTML (RawHTMLContent html) = html
