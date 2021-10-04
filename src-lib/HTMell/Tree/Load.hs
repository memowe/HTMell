{-|
Module      : HTMell.Tree.Load
Description : Content tree loading
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

This module creates content trees - represented by their root 'HNode' - from
directories, applying the neccessary "HTMell.Tree.Load.Transformations".
-}

module HTMell.Tree.Load
  ( buildTree
  ) where

import HTMell.Tree ( HNode(..), isInnerNode, processTree )
import HTMell.Tree.Load.Transformations ( indexContent, removeIndex, noEmptyLeaves )
import HTMell.Content ( HTMellContent(..) )
import HTMell.Util ( compose, splitNodePath )

import qualified Data.Map as M
import Data.Map ( Map, fromList )
import Data.Maybe ( catMaybes, isJust )
import System.FilePath ( (</>) )
import System.Directory ( doesFileExist, doesDirectoryExist, listDirectory )

-- | Creates content trees - represented by their root 'HNode' - from
-- a directory, applying the neccessary "HTMell.Tree.Load.Transformations".
buildTree
  :: HTMellContent c
  => FilePath -- ^ The directory to read the content tree from
  -> IO (Maybe (HNode c)) -- ^ The content tree ready to use, if possible
buildTree path = do rawTree <- loadTree 0 path
                    return $ case rawTree of
                      Just rt -> Just $ process rt
                      _       -> Nothing
  where process = compose $ processTree <$> reverse
          -- Tree transformations, composed and applied from top-down
          [ indexContent
          , removeIndex
          , noEmptyLeaves
          ]

-- Low-level tree-constructing, without transformations
loadTree :: HTMellContent c => Integer -> FilePath -> IO (Maybe (HNode c))
loadTree ordNum path = do isFile  <- doesFileExist path
                          isDir   <- doesDirectoryExist path
                          let result  | isFile    = leaf path
                                      | isDir     = tree path
                                      | otherwise = return Nothing
                          result
  where

    -- Creates a content 'HNode' from a single file
    leaf path = do  content <- getContent path
                    return $ case content of
                      Just c  -> Just $ HNode ordNum M.empty (Just c)
                      _       -> Nothing

    -- Creates an inner content tree 'HNode' from a directory
    tree path = do
      relAbsPairs <- map (\p -> (p, path </> p)) <$> listDirectory path
      childPairs  <- mapM childPair relAbsPairs
      let children = fromList $ catMaybes childPairs
      return $ Just $ HNode ordNum children Nothing

    -- Prepares a content tree with name as a child of its parent
    childPair (rawPath, filePath) = do
      let (ordNum, path) = splitNodePath rawPath
      child <- loadTree ordNum filePath
      return $ case child of
        Just child  -> Just (path, child)
        _           -> Nothing
