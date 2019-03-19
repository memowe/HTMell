module HTMell.Content where

import System.FilePath

data Node = Node {
        dir     :: FilePath,
        path    :: FilePath,
        home    :: String
} deriving (Show, Eq)

relPath :: Node -> FilePath
relPath (Node d p _) = makeRelative d p

pathParts :: Node -> [FilePath]
pathParts node = map dropTrailingPathSeparator $ splitPath $ relPath node
