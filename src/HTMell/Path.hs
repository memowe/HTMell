module HTMell.Path (HTPath, htpath, htconcat, (</>), htlast, htrel) where

import Data.List (intercalate, replicate)
import Data.List.Split (splitOn)

sep :: String
sep = "/"

newtype HTPath = HTPath [String]

instance Show HTPath where
    show (HTPath parts) = intercalate sep parts

instance Eq HTPath where
    p1 == p2 = show p1 == show p2

_normalize :: [String] -> [String]
_normalize = norm []
    where
        norm done [] = reverse done
        norm [] (a:rest) = norm [a] rest
        norm (x:done) (a:rest) = if a == ".." && x /= ".."
            then norm done rest
            else norm (a:x:done) rest

htpath :: String -> HTPath
htpath = HTPath . _normalize . parts
    where parts = filter (/=[]) . splitOn sep

instance Read HTPath where
    readsPrec _ str = [(htpath str, "")]

htlast :: HTPath -> String
htlast (HTPath parts) = if null parts then "" else last parts

htconcat :: HTPath -> HTPath -> HTPath
htconcat (HTPath p1) (HTPath p2) = HTPath $ _normalize (p1 ++ p2)
(</>) = htconcat

htrel :: HTPath -> HTPath -> HTPath
htrel (HTPath base) (HTPath path) = HTPath $ _htrel base path

_htrel :: [String] -> [String] -> [String]
_htrel [] path = path
_htrel _  []   = []
_htrel base@(b:bs) path@(p:ps) =
    if b == p
        then _htrel bs ps
        else replicate (length base) ".." ++ path
