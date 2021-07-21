-- | HTMell content tree related types and functions
module HTMell.Tree
    ( HNode(..)
    , HTree
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
