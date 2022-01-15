{-|
Module      : HTMell.Tree
Description : Content tree related types and functions
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

HTMell content tree related types and functions.
-}

module HTMell.Tree
  (
  -- * The content tree type
    HTree(..)
  , name
  , content
  , children
  -- * Basic tree operations
  , childList
  , isLeaf
  , isInnerNode
  -- * String representation
  , summary
  -- * Content queries
  -- $ContentQueries
  , findNode
  -- * Tree transformation
  , processTree
  ) where

import Data.List ( intercalate )
import Data.Tree ( Tree(Node), Forest, subForest )
import Control.Monad ( foldM )
import HTMell.Util ( pathParts )
import GHC.Generics (Constructor(conName))

-- | The main content tree type, having pairs of name and content as nodes
type HTree c = Tree (String, Maybe c)

-- | The name of a 'HTree' node
name :: HTree c -> String
name (Node (n, _) _) = n

-- | The content of a 'HTree' node
content :: HTree c -> Maybe c
content (Node (_, c) _) = c

-- | The children 'subForest' of a 'HTree' node
children :: Tree c -> Forest c
children = subForest

-- | All child nodes of the given tree, addressed by their name
childList :: HTree c -> [(String, HTree c)]
childList = map decorateName . children
  where decorateName = (,) =<< name

-- | True /iff/ the given 'HTree' node has no children.
isLeaf :: HTree c -> Bool
isLeaf = null . childList

-- | True /iff/ the given 'HTree' node has children.
isInnerNode :: HTree c -> Bool
isInnerNode = not . isLeaf

{-|
Very short structural summary of a given tree, considering names only,
separated by @","@, children in @"("@ and @")"@.

Example: The tree loaded from the following filesystem structure

@
content
 |--- 1_foo.md
 '--- 2_bar
       |--- 42_baz.md
       '--- 17_quux.md
@

has the 'summary' @"(foo,bar(quux,baz))"@.
-}
summary :: HTree c -> String
summary tree  | isLeaf tree = ""
              | otherwise   = "(" ++ toStr tree ++ ")"
  where toStr       = intercalate "," . map pair . childList
        pair (k, t) = k ++ summary t

-- | Bottom-up inside-out processing of a content tree. Each node will be
-- modified by the given function, possibly changing the tree's
-- 'HTMell.Content.HTMellContent' instance.
processTree
  :: (String -> Maybe a -> [HTree b] -> HTree b)
  -- ^ A function that /modifies/ a given deconstructed 'HTree', represented
  -- by the node's name ('String'), its content (@'Maybe' a@) and its child
  -- nodes as a 'Forest' of trees. The children are already modified by
  -- 'processTree' (/bottom-up/), thus already having the new content type.
  -> HTree a -- ^ The tree to /modify/.
  -> HTree b -- ^ The /modified/ tree.
processTree f node@(Node (n, c) ch) = f n c updatedCh
  where updatedCh = processTree f <$> children node

-- $ContentQueries
-- Refer to "HTMell" for an explanation of how directory and file names
-- correspond to the 'HTree's names in a content tree.

-- | Selects a node inside the given 'HTree' via its name path, separated
-- by @"/"@. For example, the @"quux"@ node in the tree above ('summary')
-- would be selected with @"\/bar\/quux"@.
findNode :: HTree c -> String -> Maybe (HTree c)
findNode tree = foldM selectChild tree . pathParts
  where selectChild = flip lookup . childList
