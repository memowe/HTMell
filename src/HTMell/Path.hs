module HTMell.Path (HTPath, htpath, htpshow, htpeq, htpconcat, (</>), htprel) where

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

htpshow :: HTPath -> String
htpshow = intercalate sep . _normalize

htpeq :: HTPath -> HTPath -> Bool
htpeq p1 p2 = htpshow p1 == htpshow p2

htpconcat :: HTPath -> HTPath -> HTPath
htpconcat p1 p2 = _normalize $ p1 ++ p2
(</>) = htpconcat

htprel :: HTPath -> HTPath -> HTPath
htprel [] path   = path
htprel _  []     = []
htprel base@(b:bs) path@(p:ps) =
    if b == p
        then htprel bs ps
        else replicate (length base) ".." ++ path
