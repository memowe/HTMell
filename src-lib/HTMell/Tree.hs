-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HNode(..)
    , childList
    , isLeaf
    , isInnerNode
    , summary
    , processTree
    , findHNode
    ) where

import qualified Data.Map as M
import Data.Map ( Map, assocs )
import Data.List ( sortOn, dropWhile, dropWhileEnd, intercalate )
import Data.List.Split ( splitOn )
import Control.Monad ( foldM )

-- | A content node/tree, maybe with children
data HNode c = HNode {
    -- | defines an order for all children of its parent
    ord :: Integer,
    -- | All children of this 'HNode', addressed by their relative paths
    children :: Map String (HNode c),
    -- | Content of this node
    content :: Maybe c
} deriving (Eq, Show)

instance (Eq c) => Ord (HNode c) where
    node1 <= node2 = ord node1 <= ord node2

childList :: (Eq c) => HNode c -> [(String, HNode c)]
-- ^ All child nodes of the given tree, addressed by their path
--   from parent, ordered by their 'ord'
childList = sortOn snd . assocs . children

isLeaf :: HNode c -> Bool
-- ^ Test if the given 'HNode' has no children
isLeaf = M.null . children

isInnerNode :: HNode c -> Bool
-- ^ Test if the given 'HNode' has children
isInnerNode = not . isLeaf

summary :: (Eq c) => HNode c -> String
-- ^ Very short structural summary of a given tree
summary tree
    | M.null $ children tree  = ""
    | otherwise             = "(" ++ toStr tree ++ ")"
    where
        toStr       = intercalate "," . map pair . childList
        pair (k, t) = k ++ summary t

processTree :: ((Integer, Map String (HNode b), Maybe a) -> HNode b) ->
    HNode a -> HNode b
-- ^ Bottom-up inside-out processing of a content tree. Each node will be modified
--   by the given function. The arguments for the given function are the ord,
--   the updated children and the content of the node to be modified.
processTree f node = f (ord node, updatedChildren, content node)
    where updatedChildren = processTree f <$> children node

findHNode :: HNode c -> String -> Maybe (HNode c)
-- ^ Allows to select a "deep" 'HNode' inside a given tree via a
--   combined path, separated by slashes. For example, this would
--   select the @"foo"@ child's child @"bar"@ in a given @tree@:
--
-- > findHNode tree "foo/bar"
findHNode tree = foldM selectChild tree . pathParts
    where
        selectChild = flip M.lookup . children
        pathParts   = splitOn "/" . trimSlashes
        trimSlashes = dropWhile (== '/') . dropWhileEnd (== '/')
