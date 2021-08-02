module HTMell.Tree.Load
    ( buildTree
    ) where

import HTMell.Tree ( HNode(..), isInnerNode, processTree )
import HTMell.Content ( HTMellContent(..) )
import HTMell.Util ( compose, splitNodePath )

import qualified Data.Map as M
import Data.Map ( Map, fromList )
import Data.Maybe ( catMaybes, isJust )
import Control.Bool ( (<||>) )
import System.FilePath ( (</>) )
import System.Directory ( doesFileExist, doesDirectoryExist, listDirectory )

buildTree :: HTMellContent c => FilePath -> IO (Maybe (HNode c))
-- ^ TODO: Loads a content tree from file system
buildTree path = do
    rawTree <- loadTree 0 path
    return $ case rawTree of
        Just rt -> Just $ process rt
        _       -> Nothing
    where
        process = compose $ processTree <$> reverse
            -- Composed and applied from top-down
            [ indexContent
            , removeIndex
            , noEmptyLeaves
            ]

loadTree :: HTMellContent c => Integer -> FilePath -> IO (Maybe (HNode c))
loadTree ordNum path = do
    isFile  <- doesFileExist path
    isDir   <- doesDirectoryExist path
    let result  | isFile    = leaf path
                | isDir     = tree path
                | otherwise = return Nothing
    result

    where

        leaf path = do
            content <- getContent path
            return $ case content of
                Just c  -> Just $ HNode ordNum M.empty (Just c)
                _       -> Nothing

        tree path = do
            relAbsPairs <- map (\p -> (p, path </> p)) <$> listDirectory path
            childPairs <- mapM childPair relAbsPairs
            let children = fromList $ catMaybes childPairs
            return $ Just $ HNode ordNum children Nothing

        childPair (rawPath, filePath) = do
            let (ordNum, path) = splitNodePath rawPath
            child <- loadTree ordNum filePath
            return $ case child of
                Just child  -> Just (path, child)
                _           -> Nothing

-- Tree processing steps -----------------------------------------------

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
