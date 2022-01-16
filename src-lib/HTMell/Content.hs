{-|
Module      : HTMell.Content
Description : HTMell content
Copyright   : (c) 2021-2022 Mirko Westermeier
License     : MIT

HTMell content related types and functions
-}

module HTMell.Content
  (
  -- * The content type class
    HTMellContent(..)
  -- * Predefined content types
  , RawHTMLContent(..)
  ) where

import System.FilePath ( FilePath, takeExtension )
import Data.Map ( Map, empty )
import Data.Text ( Text )
import Prelude hiding ( readFile )
import Data.Text.IO ( readFile )

-- | Content stored in 'HTMell.Tree.HTree' trees. Instances must provide
-- a function to read content from filesystem and extract metadata and HTML
-- from a content value.
class HTMellContent c where
  getContent :: FilePath -> IO (Maybe c)
  -- ^ Extract content from a given file. The instance can ignore the file
  -- with 'Nothing', for example if it has the wrong extension.
  metadata :: c -> Map String String
  -- ^ Extract metadata from a given content value.
  toHTML :: c -> Text
  -- ^ Extract plain HTML 'Text' from a given content value.

-- | The simplest possible useful content type as the extracted /HTML/ via
-- 'toHTML'is just the file content. Ignores files that have no @".html"@
-- extension and extracts an 'empty' metadata 'Map' via 'metadata'.
newtype RawHTMLContent = RawHTMLContent Text deriving (Eq, Show)

isRawHTML :: FilePath -> Bool
isRawHTML = (== ".html") . takeExtension

instance HTMellContent RawHTMLContent where
  getContent fp | isRawHTML fp  = Just . RawHTMLContent <$> readFile fp
                | otherwise     = return Nothing
  metadata = const empty
  toHTML (RawHTMLContent html) = html
