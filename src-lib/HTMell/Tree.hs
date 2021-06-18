module HTMell.Tree
    ( HNode(..)
    , HTree(..)
    , HDoc(..)
    , parseHNode
    ) where

import Text.ParserCombinators.ReadP (ReadP, char, munch1, option, readP_to_S)
import Data.Maybe (fromMaybe)

data HNode = HNode
    { path :: String
    , ord :: Integer
    } deriving (Eq, Show)

data HTree
    = HLeaf HNode HDoc
    | HInnerNode HNode [HTree]
    deriving (Eq, Show)

data HDoc = HDoc
    { filepath :: FilePath
    } deriving (Eq, Show)

digits_ :: ReadP Integer
digits_ = do
    ord <- read <$> munch1 (`elem` ['0'..'9'])
    char '_'
    return ord

hnodeP :: ReadP HNode
hnodeP = do
    ord <- option Nothing $ Just <$> digits_
    path <- munch1 (/= '.')
    return $ HNode path $ fromMaybe 0 ord

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        []              -> Nothing
        ((result, _):_) -> Just result

parseHNode :: String -> Maybe HNode
parseHNode = parseMaybe hnodeP
