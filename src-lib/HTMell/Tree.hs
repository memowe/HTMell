module HTMell.Tree
    ( HNode(..)
    , HTree(..)
    , parseHNode
    ) where

import Data.Tree ( Tree )
import Data.Maybe ( fromMaybe )
import Text.ParserCombinators.ReadP ( ReadP, char, munch1, option, readP_to_S )

data HNode = HNode
    { path :: String
    , ord :: Integer
    } deriving (Eq, Show)

type HTree = Tree HNode

digits_ :: ReadP Integer
digits_ = do
    ord <- read <$> munch1 (`elem` ['0'..'9'])
    char '_'
    return ord

hnodeP :: ReadP HNode
hnodeP = do
    ord <- option Nothing $ Just <$> digits_
    path <- munch1 (const True)
    return $ HNode path $ fromMaybe 0 ord

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        []              -> Nothing
        ((result, _):_) -> Just result

parseHNode :: String -> Maybe HNode
parseHNode = parseMaybe hnodeP
