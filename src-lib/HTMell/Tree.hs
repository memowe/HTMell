-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HTree(..)
    , HNode
    , summary
    , findHNode
    ) where

import qualified Data.Map as M
import Data.Map ( Map, lookup, assocs )
import Data.List ( dropWhile, dropWhileEnd, intercalate )
import Data.List.Split ( splitOn )
import Control.Monad ( foldM )

-- | A content tree, maybe with children
data HTree = HTree {
    -- | defines an order for all children of its parent
    ord :: Integer,
    -- | All children of this 'HNode', addressed by their relative paths
    children :: Map String HNode
} deriving (Eq, Show)

-- | 'HTree' alias for readability
type HNode = HTree

summary :: HTree -> String
-- ^ Very short structural summary of a given 'HTree'
summary (HTree _ children)
    | null children = ""
    | otherwise     = "(" ++ toStr children ++ ")"
    where
        toStr       = intercalate "," . map pair . assocs
        pair (k, t) = k ++ summary t

findHNode :: HTree -> String -> Maybe HNode
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
