module HTMell.Path (HTPath, htpath, htshow, hteq, htconcat, (</>), htrel) where

import Data.List (intercalate, replicate)
import Data.List.Split (splitOn)

sep :: String
sep = "/"

type HTPath = [String]

_normalize :: HTPath -> HTPath
_normalize = norm []
    where
        norm done [] = reverse done
        norm [] (a:rest) = norm [a] rest
        norm (x:done) (a:rest) = if a == ".." && x /= ".."
            then norm done rest
            else norm (a:x:done) rest

htpath :: String -> HTPath
htpath = _normalize . parts
    where parts = filter (/=[]) . splitOn sep

htshow :: HTPath -> String
htshow = intercalate sep . _normalize

hteq :: HTPath -> HTPath -> Bool
hteq p1 p2 = htshow p1 == htshow p2

htconcat :: HTPath -> HTPath -> HTPath
htconcat p1 p2 = _normalize $ p1 ++ p2
(</>) = htconcat

htrel :: HTPath -> HTPath -> HTPath
htrel [] path   = path
htrel _  []     = []
htrel base@(b:bs) path@(p:ps) =
    if b == p
        then htrel bs ps
        else replicate (length base) ".." ++ path
