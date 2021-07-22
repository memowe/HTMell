-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HNode(..)
    , summary
    , findHNode
    ) where

import qualified Data.Map as M
import Data.Map ( Map, lookup, assocs )
import Data.List ( dropWhile, dropWhileEnd, intercalate )
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

summary :: HNode c -> String
-- ^ Very short structural summary of a given 'HTree'
summary (HNode _ children _)
    | null children = ""
    | otherwise     = "(" ++ toStr children ++ ")"
    where
        toStr       = intercalate "," . map pair . assocs
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
