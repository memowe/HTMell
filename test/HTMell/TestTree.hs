module HTMell.TestTree ( testTree ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Tree ( HTree(..), name, content, children, isLeaf, isInnerNode, childList, summary, drawHTree, processTree, findNode )
import HTMell.Content ( RawHTMLContent (RawHTMLContent), toHTML )
import HTMell.Util ( cempty, PseudoContent(..) )
import Data.Maybe ( isNothing, fromJust )
import Data.List ( sortOn )
import Data.Tree ( Tree(Node) )
import qualified Data.Text as T
import Data.Text ( Text, pack )

trivialTree :: HTree PseudoContent
trivialTree = Node ("", Nothing) []
childTree   = Node ("", cempty) [Node ("foo", cempty) []]
exampleTree = Node ("", cempty)
  [ Node ("foo", cempty)
    [ Node ("bidu", cempty) []
    , Node ("bar", cempty)
      [ Node ("baz", cempty) []
      , Node ("quux", cempty) []
      ]
    ]
  , Node ("xnorfzt", cempty) []
  ]

etFoo     = head        $ children exampleTree
etBidu    = head        $ children etFoo
etBar     = head $ tail $ children etFoo
etBaz     = head        $ children etBar
etQuux    = head $ tail $ children etBar
etXnorfzt = head $ tail $ children exampleTree

testLeaf = testGroup "Leaf/Inner node"
  [ testCase "Empty tree leaf" $ isLeaf trivialTree @?= True
  , testCase "Empty tree inner node" $ isInnerNode trivialTree @?= False
  , testCase "Single child leaf" $ isLeaf childTree @?= False
  , testCase "Single child inner node" $ isInnerNode childTree @?= True
  , testCase "Complex tree leaf is leaf" $ isLeaf etBaz @?= True
  , testCase "Complex tree leaf is not inner node" $ isInnerNode etBaz @?= False
  , testCase "Complex tree inner node is not leaf" $ isLeaf etBar @?= False
  , testCase "Complex tree inner node is inner node" $ isInnerNode etBar @?= True
  ]

testSummary = testGroup "Tree summary"
  [ testCase "Empty tree" $ summary trivialTree @?= ""
  , testCase "Single child" $ summary childTree @?= "(foo)"
  , testCase "Complex tree" $
      summary exampleTree @?= "(foo(bidu,bar(baz,quux)),xnorfzt)"
  ]

-- Simple test only as this should be tested in Data.Tree
testDrawTree = testCase "Tree drawing (Data.Tree)" $ drawHTree exampleTree
  @?= "|\n\
      \+- foo\n\
      \|  |\n\
      \|  +- bidu\n\
      \|  |\n\
      \|  `- bar\n\
      \|     |\n\
      \|     +- baz\n\
      \|     |\n\
      \|     `- quux\n\
      \|\n\
      \`- xnorfzt\n"

testChildList = testGroup "List of children"
  [ testCase "Empty tree" $ childList trivialTree @?= []
  , testCase "Single child" $
      childList childTree @?= [("foo", Node ("foo", cempty) [])]
  , testCase "Complex tree 'foo' children" $
      childList etFoo @?= [("bidu", etBidu), ("bar", etBar)]
  , testCase "Complex tree 'foo/bar' children" $
      childList etBar @?= [("baz", etBaz), ("quux", etQuux)]
  ]

-- Process example tree in a non-trivial way
processor
  :: String -> Maybe PseudoContent -> [HTree RawHTMLContent]
  -> HTree RawHTMLContent
processor n _ cs = Node (newName, Just newContent) newChs
  where newName     = reverse n ++ show (length cs)
        newContent  = RawHTMLContent $ T.pack $ unwords chNames
        chNames     = map name cs
        newChs      = if null cs then [answer] else reverse cs
        answer      = Node ("42", Just $ RawHTMLContent $ T.pack "answer") []

processedTree = processTree processor exampleTree

testTreeProcessing = testGroup "Tree processing"
  [ testCase "Correct structure" $ summary processedTree @?= "\
    \(tzfronx0(42)\
    \,oof2\
      \(rab2\
        \(xuuq0(42)\
        \,zab0(42)\
        \)\
      \,udib0(42)\
      \)\
    \)"
  , testCase "Root foo content" $
      toHTML (fromJust $ content $ fromJust $ findNode processedTree "oof2")
        @?= T.pack "udib0 rab2"
  , testCase "Root foo/bar content" $
      toHTML (fromJust $ content $ fromJust $ findNode processedTree "oof2/rab2")
        @?= T.pack "zab0 xuuq0"
  , testCase "Empty xnorfzt content" $
      toHTML (fromJust $ content $ fromJust $ findNode processedTree "tzfronx0/42")
        @?= T.pack "answer"
  ]

-- Helper operator for simplified summary testing of HTrees
a @?=| b = summary (fromJust a) @?= b

testFindNode = testGroup "Find HTrees"
  [ testCase "Empty tree" $
      findNode trivialTree "foo" @?= Nothing
  , testCase "Empty query: root node" $
      findNode childTree "" @?= Just childTree
  , testCase "Direct child" $
      findNode childTree "foo" @?= Just (Node ("foo", cempty) [])
  , testCase "Complex subtree query" $
      findNode exampleTree "foo/bar" @?=| "(baz,quux)"
  , testCase "Complex leaf query" $
      findNode exampleTree "foo/bar/quux" @?=| ""
  ]

testTree = testGroup "Content tree tests"
  [ testLeaf
  , testSummary
  , testDrawTree
  , testChildList
  , testTreeProcessing
  , testFindNode
  ]
