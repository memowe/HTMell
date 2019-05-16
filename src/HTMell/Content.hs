module HTMell.Content where

import System.FilePath
import Text.Regex.PCRE

data Node = Node {
    dir     :: FilePath,
    path    :: FilePath,
    home    :: String
} deriving (Show, Eq)

relPath :: Node -> FilePath
relPath (Node d p _) = makeRelative d p

pathParts :: Node -> [FilePath]
pathParts = (map dropTrailingPathSeparator) . splitPath . relPath

baseName :: Node -> FilePath
baseName = last . pathParts

data NodeNameParts = NodeNameParts {
    sortVal     :: Int,
    pathName    :: String,
    fileExt     :: String
} deriving (Show)

nameParts :: Node -> NodeNameParts
nameParts node = buildNameParts captures
    where fileNameRx = "^(?:(\\d+)_)?(.+?)(?:\\.(md|markdown))?$"
          captures   = head (baseName node =~ fileNameRx :: [[String]])
          buildNameParts (_:sv:pn:fe:[]) = NodeNameParts (read sv :: Int) pn fe

-- Example data
nod = Node "foo/bar" "foo/bar/baz/42_quux.md" "homie"
