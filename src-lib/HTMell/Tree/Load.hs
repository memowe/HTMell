module HTMell.Tree.Load
    ( loadTree
    ) where

import HTMell.Tree ( HNode(..), isLeaf, processTree )
import HTMell.Content ( HTMellContent(..) )
import HTMell.Util ( splitNodePath )

import qualified Data.Map as M
import Data.Map ( Map, fromList )
import Data.Maybe ( catMaybes, isNothing )
import Control.Bool ( (<&&>) )
import System.FilePath ( (</>) )
import System.Directory ( doesFileExist, doesDirectoryExist, listDirectory )

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
