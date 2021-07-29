-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HNode(..)
    , childList
    , summary
    , findHNode
    ) where

import qualified Data.Map as M
import Data.Map ( Map, lookup, assocs )
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
    content :: c
} deriving (Eq, Show)

instance (Eq c) => Ord (HNode c) where
    node1 <= node2 = ord node1 <= ord node2

childList :: (Eq c) => HNode c -> [(String, HNode c)]
-- ^ All child nodes of the given tree, addressed by their path
--   from parent, ordered by their 'ord'
childList = sortOn snd . assocs . children

summary :: (Eq c) => HNode c -> String
-- ^ Very short structural summary of a given 'HTree'
summary tree
    | null $ children tree  = ""
    | otherwise             = "(" ++ toStr tree ++ ")"
    where
        toStr       = intercalate "," . map pair . childList
        pair (k, t) = k ++ summary t

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
