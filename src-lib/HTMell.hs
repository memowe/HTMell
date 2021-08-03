-- | Minimal filesystem powered markdown content management system

module HTMell
  (
  -- * Loading a content tree from filesystem
    loadHTMell
  -- * Accessing single content (nodes)
  , get
  , getHTML
  ) where

import HTMell.Content ( HTMellContent(..) )
import HTMell.Tree ( HNode(..), findHNode )
import HTMell.Tree.Load ( buildTree )
import Data.Text ( Text )

-- | For a given 'FilePath', load a content tree from that directory.
-- Each file or subdirectory of that content directory can become a content
-- node in the resulting tree. Subdirectories can become inner nodes with
-- children, but they can also have content, that is loaded from their
-- @index@ child, if it exists. Directory or file names become the node\'s
-- addresses in the content tree. Leading numbers, followed by @"_"@ are
-- used for sorting in listings, and are stripped from the names, as well
-- as extensions.
--
-- Example:
--
-- > Filesystem:                Content tree:
-- >
-- > content                    Content root node
-- >  |--- 1_foo.md             Content node "/foo"
-- >  |--- index.md             Content of the root node "/"
-- >  '--- 2_bar                Inner node "/bar", no content
-- >        |--- 42_baz.md      Content node "/bar/baz"
-- >        '--- 17_quux.md     Content node "/bar/quux"
--
-- In listings, the node @"\/bar\/quux"@ will appear before @"\/bar\/baz"@
-- because of the given ordering numbers. What file extensions are used or
-- ignored is up to the used 'HTMellContent' instance.
-- 'HTMell.Content.RawHTMLContent' for example reads only @.html@ files.
loadHTMell :: HTMellContent c => FilePath -> IO (Maybe (HNode c))
loadHTMell = buildTree

-- | Finds a subtree or leaf (content node) in a given content tree. The
-- query 'String' consists of child node names, separated with @"\/"@.
-- See 'loadHTMell' for details of node names.
get
  :: HTMellContent c
  => HNode c -- ^ The content tree to search in
  -> String -- ^ A query string of the desired subtree or content node
  -> Maybe (HNode c) -- ^ The desired content node, if it exists
get = findHNode

-- | The same as 'get', but accessing the 'content' of the desired content
-- node directly.
getHTML :: HTMellContent c => HNode c -> String -> Maybe Text
getHTML tree query = do
  node <- get tree query
  toHTML <$> content node
