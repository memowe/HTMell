module HTMell.SimpleFrontmatter (splitFrontmatter) where

import Data.Map (Map, empty, insert)
import Text.Regex.TDFA ((=~))

splitFrontmatter :: String -> (Map String String, String)
splitFrontmatter = _split empty . lines

_split :: Map String String -> [String] -> (Map String String, String)
_split pairs lines@(line:rest)
    | isPair    = _split (uncurry insert getPair pairs) rest
    | isLine    = (pairs, unlines rest)
    | otherwise = (pairs, unlines lines)
    where
        sepRx   = "^ *---+ *$"
        pairRx  = "^ *([^ =]+) *= *(.*)$"
        isLine  = line =~ sepRx :: Bool
        isPair  = line =~ pairRx :: Bool
        getPair = let [[_, k, v]] = line =~ pairRx :: [[String]] in (k, v)
