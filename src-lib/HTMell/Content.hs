-- | HTMell content related types and functions
module HTMell.Content
    ( HTMellContent(..)
    ) where

import System.FilePath ( FilePath )
import Data.Map ( Map )
import Data.Text ( Text )

-- | Some content stored in 'HTMell.Tree.HNode' trees
class HTMellContent c where
    getContent :: FilePath -> Maybe c
    metadata :: c -> Map String String
    html :: c -> Text
