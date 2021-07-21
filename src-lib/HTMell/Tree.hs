-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HNode(..)
    , HTree
    , findHNode
    ) where

import qualified Data.Map as M
import Data.Map ( Map, lookup )
import Data.List ( dropWhile, dropWhileEnd )
import Data.List.Split ( splitOn )
import Control.Monad ( foldM )

-- | A content node, maybe with children
data HNode = HNode {
    -- | defines an order for all children of its parent
    ord :: Integer,
    -- | All children of this 'HNode', addressed by their relative paths
    children :: Map String HNode
} deriving (Eq, Show)

-- | 'HNode' alias for readability for its property to have children
type HTree = HNode

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
