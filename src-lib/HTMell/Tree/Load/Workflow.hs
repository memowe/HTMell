module HTMell.Tree.Load.Workflow
    ( indexContent
    , removeIndex
    , noEmptyLeaves
    ) where

import HTMell.Tree ( HNode(..), isInnerNode )
import qualified Data.Map as M
import Data.Maybe ( isJust )
import Control.Bool ( (<||>) )

-- Just a little helper for tree processors that only modify child maps
childProcess f (o, ch, c) = HNode o (f ch) c

indexContent (o, ch, c) = HNode o ch $
    case c of
        Nothing -> indexContent
        other   -> other
    where indexContent = do
            child <- M.lookup "index" ch
            content child

removeIndex = childProcess $
    M.filterWithKey $ const . (/= "index")

noEmptyLeaves = childProcess $
    M.filter $ isInnerNode <||> isJust . content
