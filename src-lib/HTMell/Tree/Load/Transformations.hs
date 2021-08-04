{-|
Module      : HTMell.Tree.Load.Transformations
Description : Content tree transformations
Copyright   : (c) 2021 Mirko Westermeier
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

import HTMell.Tree ( HNode(..), isLeaf )
import qualified Data.Map as M
import Data.Maybe ( isNothing )
import Control.Bool ( notF, (<&&>) )

-- Just a little helper for tree processors that only modify child maps
childProcess f (o, ch, c) = HNode o (f ch) c

{- $InnerNodeContent
While a leaf of the content tree gets its content from a file, it is not
immediately clear for (inner) directories that they can also contain
content. @"index"@ children to the rescue!

__Caveat__: Don't use /directories/ named @"index"@!
-}

-- | Attaches the 'content' of a content tree leaf to its parent 'HNode' if
-- its 'children' name is @"index"@.
indexContent (o, ch, c) = HNode o ch $ case c of
  Nothing -> content =<< M.lookup "index" ch
  other   -> other

-- | Removes all 'children' named @"index"@.
removeIndex = childProcess $
  M.filterWithKey $ const . (/= "index")

-- | Removes content tree leaves with @Nothing@ as their 'content'.
noEmptyLeaves = childProcess $
  M.filter $ notF $ isLeaf <&&> isNothing . content