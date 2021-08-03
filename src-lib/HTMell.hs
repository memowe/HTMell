module HTMell
  ( loadHTMell
  , get
  , getHTML
  ) where

import HTMell.Content ( HTMellContent(..) )
import HTMell.Tree ( HNode(..), findHNode )
import HTMell.Tree.Load ( buildTree )
import Data.Text ( Text )

loadHTMell :: HTMellContent c => FilePath -> IO (Maybe (HNode c))
loadHTMell = buildTree

get :: HTMellContent c => HNode c -> String -> Maybe (HNode c)
get = findHNode

getHTML :: HTMellContent c => HNode c -> String -> Maybe Text
getHTML tree query = do
  node <- get tree query
  toHTML <$> content node
