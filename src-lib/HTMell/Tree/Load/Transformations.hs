{-|
Module      : HTMell.Tree.Load.Transformations
Description : Content tree transformations
Copyright   : (c) 2021-2022 Mirko Westermeier
License     : MIT

This module is a collection of all tree modifications used in
'HTMell.Tree.Load.buildTree' (and 'HTMell.loadHTMell'). They are applied
in a chain of 'HTMell.Tree.processTree' calls and do not need to be used
outside of HTMell. For details on the somewhat strange signatures, refer
to 'HTMell.Tree.processTree'.
-}

module HTMell.Tree.Load.Transformations
  (
  -- * Content of inner nodes
  -- $InnerNodeContent
    indexContent
  , removeIndex
  -- * Tree cleansing
  , noEmptyLeaves
  ) where

import Data.List ( find )
import Data.Maybe ( isNothing )
import Data.Tree ( Tree(Node) )
import HTMell.Tree ( HTree, name, content, children, isLeaf )

-- Just a little helper for tree processors that only modify child maps
childProcess f n c ch = Node (n, c) (f ch)

{- $InnerNodeContent
While a leaf of the content tree gets its content from a file, it is not
immediately clear for (inner) directories that they can also contain
content. @"index"@ children to the rescue!

__Caveat__: Don't use /directories/ named @"index"@!
-}

-- | Attaches the 'content' of a content tree leaf to its parent 'HTree' if
-- its 'children' name is @"index"@.
indexContent n c ch = Node (n, c') ch
  where c' = case c of
              Nothing -> content =<< find ((== "index") . name) ch
              other   -> other

-- | Removes all 'children' named @"index"@.
removeIndex = childProcess $
  filter ((/= "index") . name)

-- | Removes content tree leaves with @Nothing@ as their 'content'.
noEmptyLeaves = childProcess $
  filter (not . ((&&) <$> isLeaf <*> isNothing . content))
