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
    HNode(..)
  -- * Basic tree operations
  , childList
  , isLeaf
  , isInnerNode
  -- * String representation
  , summary
  -- * Content queries
  -- $ContentQueries
  , findHNode
  -- * Tree transformation
  , processTree
  ) where

import qualified Data.Map as M
import Data.Map ( Map, assocs )
import Data.List ( sortOn, dropWhile, dropWhileEnd, intercalate )
import Data.List.Split ( splitOn )
import Data.Function ( on )
import Control.Monad ( foldM )

-- | The main content tree type, represented by its root node 'HNode'.
data HNode c = HNode {
  ord :: Integer, -- ^ Defines a position within the sequence of sibling
    -- 'HNode's as 'children' of the parent.
  children :: Map String (HNode c), -- ^ All children of this 'HNode',
    -- addressed by their (processed) relative paths inside the filesystem.
  content :: Maybe c -- ^ Content of this node, read from file contents.
} deriving
  ( Eq -- ^ Default 'Eq' instance.
  , Show -- ^ Default 'Show' instance.
  )

-- | 'HNode's are ordered according to their 'ord' numbers.
instance (Eq c) => Ord (HNode c) where
  (<=) = (<=) `on` ord

-- | All 'children' 'HNode's of the given tree, addressed by their name,
-- ordered by their 'ord'.
childList :: (Eq c) => HNode c -> [(String, HNode c)]
childList = sortOn snd . assocs . children

-- | True /iff/ the given 'HNode' has no 'children'.
isLeaf :: HNode c -> Bool
isLeaf = M.null . children

-- | True /iff/ the given 'HNode' has 'children'.
isInnerNode :: HNode c -> Bool
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
summary :: (Eq c) => HNode c -> String
summary tree  | isLeaf tree = ""
              | otherwise   = "(" ++ toStr tree ++ ")"
  where toStr       = intercalate "," . map pair . childList
        pair (k, t) = k ++ summary t

-- | Bottom-up inside-out processing of a content tree. Each node will be
-- modified by the given function, possibly changing the tree's
-- 'HTMell.Content.HTMellContent' instance.
processTree
  :: ((Integer, Map String (HNode b), Maybe a) -> HNode b)
  -- ^ A function that /modifies/ a given deconstructed 'HNode', represented
  -- by a triple of an 'Integer' ('ord'), a 'Map' ('children') and a
  -- @'Maybe' a@ ('content'). The /children/ are already modified by
  -- 'processTree' (/bottom-up/), thus already having the new content type.
  -> HNode a -- ^ The node to /modify/.
  -> HNode b -- ^ The /modified/ node.
processTree f node = f (ord node, updatedChildren, content node)
  where updatedChildren = processTree f <$> children node

-- $ContentQueries
-- Refer to "HTMell" for an explanation of how directory and file names
-- correspond to the 'HNode's names in a content tree.

-- | Selects a 'HNode' inside the given tree via its name path, separated
-- by @"/"@. For example, the @"quux"@ node in the tree above ('summary')
-- would be selected with @"\/bar\/quux"@.
findHNode :: HNode c -> String -> Maybe (HNode c)
findHNode tree = foldM selectChild tree . pathParts
  where selectChild = flip M.lookup . children
        pathParts   = splitOn "/" . trimSlashes
        trimSlashes = dropWhile (== '/') . dropWhileEnd (== '/')
