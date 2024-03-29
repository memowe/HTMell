{-|
Module      : HTMell
Description : Minimal filesystem powered markdown content management system
Copyright   : (c) 2021-2022 Mirko Westermeier
License     : MIT
Stability   : experimental

Minimal filesystem powered markdown content management system
-}

module HTMell
  (
  -- * Loading a content tree from filesystem
  -- $intro
    loadHTMell
  , loadHTMellContent
  -- * Accessing single content nodes
  , get
  , getHTML
  ) where

import HTMell.Content ( HTMellContent(..) )
import HTMell.Content.Markdown ( MarkdownContent )
import HTMell.Tree ( HTree(..), content, findNode )
import HTMell.Tree.Load ( buildTree )
import Data.Text ( Text )

{- $intro
A content tree can be built from any directory, given by a 'FilePath'. Each
file or subdirectory in that content directory can become a content node in
the resulting tree. Subdirectories can become inner nodes with children, but
they can also have content, that is loaded from their @"index"@ child, if it
exists. Directory or file names become the node\'s addresses in the content
tree. Leading numbers, followed by @"_"@ are used for sorting in listings,
and are stripped from the names, as well as extensions.

Example:

@
Filesystem:                 Content tree:

content                     Content root node
  |--- 1_foo.md             Content node "\/foo"
  |--- index.md             Content of the root node "\/"
  '--- 2_bar                Inner node "\/bar", no content
        |--- 17_baz.md      Content node "\/bar\/baz"
        |--- 42_quux.md     Content node "\/bar\/quux"
        '--- xnorfzt.md     Content node "\/bar\/xnorfzt"
@

__Caveat__: Don't use /directories/ named @"index"@!

In listings, the nodes below @"\/bar"@ will appear in this order, defined by
their (optional) ordering numbers:

  [@\/bar\/xnorfzt@]
  because it's loaded from @xnorfzt.md@ and no number implies 0
  [@\/bar\/baz@]
  because it's loaded from @17_baz.md@ and 17 < 42
  [@\/bar\/quux@]
  because it's loaded from @42_quux.md@ and 42 > 17

The accepted file extensions are the responsibility of the 'HTMellContent'
instance used. The default instance, 'MarkdownContent', accepts @.md@,
'HTMell.Content.RawHTMLContent' reads @.html@ files only.
-}

-- | Creates a markdown content tree
loadHTMell
  :: FilePath
  -- ^ The directory to read the content tree from.
  -> IO (Maybe (HTree MarkdownContent))
  -- ^ The loaded content tree, if possible.
loadHTMell = buildTree

-- | Creates a content tree just like 'loadHTMell', but it works for any
-- 'HTMellContent' instance, for example 'HTMell.Content.RawHTMLContent'.
-- The type needs to be declared:
--
-- > maybeTree <- loadHTMell "content" :: IO (Maybe (HTree RawHTMLContent))
-- > let tree = fromJust maybeTree
loadHTMellContent :: HTMellContent c => FilePath -> IO (Maybe (HTree c))
loadHTMellContent = buildTree

-- | Finds a subtree or leaf (content node) in a given content tree. The
-- query 'String' consists of child node names, separated with @"\/"@.
-- See 'loadHTMell' for details of node names.
get
  :: HTMellContent c
  => HTree c -- ^ The content tree to search in
  -> String -- ^ A query string of the desired subtree or content node
  -> Maybe (HTree c) -- ^ The desired content node, if it exists
get = findNode

-- | The same as 'get', but accessing the 'content' of the desired content
-- node directly.
getHTML :: HTMellContent c => HTree c -> String -> Maybe Text
getHTML tree query = do node <- get tree query
                        toHTML <$> content node
