-- | HTMell content related types and functions
module HTMell.Content
    ( HTMellContent(..)
    ) where

import System.FilePath ( FilePath )
import Data.Map ( Map )
import Data.Text ( Text )

-- | Some content stored in 'HTMell.Tree.HNode' trees
class HTMellContent c where
    -- | Extract a content representation
    getContent :: FilePath -> Maybe c
    -- | Some simple metadata
    metadata :: c -> Map String String
    -- | Translate our content to plain HTML 'Text'
    toHTML :: c -> Text
