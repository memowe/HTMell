{-# LANGUAGE TupleSections #-}
{-|
Module      : HTMell.Tree.Load
Description : Content tree loading
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

This module creates content trees - represented by their root 'HTree' - from
directories, applying the neccessary "HTMell.Tree.Load.Transformations".
-}

module HTMell.Tree.Load
  ( buildTree
  ) where

import HTMell.Tree ( HTree(..), isInnerNode, processTree )
import HTMell.Tree.Load.Transformations ( indexContent, removeIndex, noEmptyLeaves )
import HTMell.Content ( HTMellContent(getContent) )
import HTMell.Util ( compose, splitNodePath )

import Data.List ( sortOn )
import Data.Maybe ( mapMaybe )
import Data.Tree ( Tree(Node) )
import System.FilePath ( (</>) )
import System.Directory ( doesFileExist, doesDirectoryExist, listDirectory )

-- | Creates 'HTree' content trees from a directory,
-- applying the neccessary "HTMell.Tree.Load.Transformations".
buildTree
  :: HTMellContent c
  => FilePath -- ^ The directory to read the content tree from
  -> IO (Maybe (HTree c)) -- ^ The content tree ready to use, if possible
buildTree path = do rawTree <- loadTree "" path
                    return $ process <$> rawTree
  where process = compose $ map processTree $ reverse
          -- Tree transformations, composed and applied from top-down
          [ indexContent
          , removeIndex
          , noEmptyLeaves
          ]

-- Low-level tree-constructing, without transformations
loadTree :: HTMellContent c => String -> FilePath -> IO (Maybe (HTree c))
loadTree name path = do isFile  <- doesFileExist path
                        isDir   <- doesDirectoryExist path
                        let node  | isFile    = leaf name path
                                  | isDir     = tree name path
                                  | otherwise = return Nothing
                        node
  where

    -- Creates a content 'HTree' from a single file
    leaf :: HTMellContent c => String -> FilePath -> IO (Maybe (HTree c))
    leaf name path = do
      content <- getContent path
      return $ flip Node [] . (name,) . Just <$> content

    -- Creates an inner content tree 'HTree' from a directory
    tree :: HTMellContent c => String -> FilePath -> IO (Maybe (HTree c))
    tree name path = do
      relAbs    <- map (\p -> (p, path </> p)) <$> listDirectory path
      children  <- mapMaybe snd . sortOn fst <$> mapM childOrd relAbs
      return $ Just $ Node (name, Nothing) children

    -- Prepares a content tree with name as a child of its parent
    childOrd :: HTMellContent c => (FilePath, FilePath) -> IO (Integer, Maybe (HTree c))
    childOrd (rawPath, filePath) = do
      let (ordNum, path) = splitNodePath rawPath
      (ordNum,) <$> loadTree path filePath

    -- Injects a name into a 'HTree' node
    setName :: String -> HTree c -> HTree c
    setName name (Node (_, c) ch) = Node (name, c) ch
