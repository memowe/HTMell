module HTMell.TestTree ( testTree ) where

import Test.Tasty ( testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import HTMell.Tree ( HNode(..), isLeaf, isInnerNode, childList, summary, processTree, findHNode )
import HTMell.Content ( RawHTMLContent(..), toHTML )
import HTMell.Util ( cempty, PseudoContent(..) )
import qualified Data.Map as M
import Data.Map ( Map, empty, null, (!), fromList, insert, assocs )
import Data.Maybe ( isNothing, fromJust )
import Data.List ( sortOn )
import qualified Data.Text as T
import Data.Text ( Text, pack )

trivialTree = HNode 42 empty cempty
childTree   = HNode 17 (fromList [("foo", HNode 42 empty cempty)]) cempty
exampleTree = HNode 17 (fromList [
        ("foo", HNode 42 (fromList [
            ("bar", HNode 37 (fromList [
                ("baz", HNode 108 empty cempty),
                ("quux", HNode 109 empty cempty)
            ]) cempty),
            ("bidu", HNode 25 empty cempty)
        ]) cempty),
        ("xnorfzt", HNode 666 empty cempty)
    ]) cempty

testLeaf = testGroup "Leaf/Inner node"
    [ testCase "Empty tree leaf" $ isLeaf trivialTree @?= True
    , testCase "Empty tree inner node" $ isInnerNode trivialTree @?= False
    , testCase "Single child leaf" $ isLeaf childTree @?= False
    , testCase "Single child inner node" $ isInnerNode childTree @?= True
    , testCase "Complex tree leaf is leaf" $
        isLeaf (fromJust $ findHNode exampleTree "foo/bar/baz") @?= True
    , testCase "Complex tree leaf is not inner node" $
        isInnerNode (fromJust $ findHNode exampleTree "foo/bar/baz") @?= False
    , testCase "Complex tree inner node is not leaf" $
        isLeaf (fromJust $ findHNode exampleTree "foo/bar") @?= False
    , testCase "Complex tree inner node is inner node" $
        isInnerNode (fromJust $ findHNode exampleTree "foo/bar") @?= True
    ]

testSummary = testGroup "Tree summary"
    [ testCase "Empty tree" $ summary trivialTree @?= ""
    , testCase "Single child" $ summary childTree @?= "(foo)"
    , testCase "Complex tree" $
        summary exampleTree @?= "(foo(bidu,bar(baz,quux)),xnorfzt)"
    ]

testChildList = testGroup "Sorted list of children"
    [ testCase "Empty tree" $ childList trivialTree @?= []
    , testCase "Single child" $
        childList childTree @?= [("foo", children childTree ! "foo")]
    , testCase "Complex tree 'foo' children" $
        childList foo @?= map (\p -> (p, children foo ! p)) ["bidu", "bar"]
    , testCase "Complex tree 'foo/bar' children" $
        childList bar @?= map (\p -> (p, children bar ! p)) ["baz", "quux"]
    ]
    where
        foo = children exampleTree ! "foo"
        bar = children foo ! "bar"

-- Process example tree in a non-trivial way
processor ::
    (Integer, Map String (HNode RawHTMLContent), Maybe PseudoContent) ->
    HNode RawHTMLContent
processor (o, cs, _) = HNode newOrd newChildren newContent
    where
        newOrd              = o + 1
        nonEmptyChildren    = if M.null cs then addAnswer cs else cs
        newChildren         = withOrdKeys nonEmptyChildren
        newContent          = Just $ RawHTMLContent $ keysText cs
        withOrdKeys         = fromList . map addOrdToKey . assocs
        addOrdToKey (p, n)  = (p ++ show (ord n), n)
        addAnswer           = insert "answer" (HNode 50 empty Nothing)
        keysText            = T.pack . unwords . map fst . sortOn snd . assocs

processedTree = processTree processor exampleTree

testTreeProcessing = testGroup "Tree processing"
    [ testCase "Correct structure" $ summary processedTree @?= "\
        \(\
            \foo43(\
                \bidu26(answer50),\
                \bar38(\
                    \baz109(answer50),\
                    \quux110(answer50)\
                \)\
            \),\
            \xnorfzt667(answer50)\
        \)"
    , testCase "Root foo content" $
        toHTML (fromJust $ content $ fromJust $ findHNode processedTree "foo43")
            @?= T.pack "bidu bar"
    , testCase "Root foo/bar content" $
        toHTML (fromJust $ content $ fromJust $ findHNode processedTree "foo43/bar38")
            @?= T.pack "baz quux"
    ]

-- Helper operator for simplified summary testing of HNodes
a @?=| b = summary (fromJust a) @?= b

testFindHNode = testGroup "Find HNodes"
    [ testCase "Empty tree" $
        findHNode trivialTree "foo" @?= Nothing
    , testCase "Empty query" $
        findHNode childTree "" @?= Nothing
    , testCase "Direct child" $
        findHNode childTree "foo" @?= Just (HNode 42 empty cempty)
    , testCase "Complex subtree query" $
        findHNode exampleTree "foo/bar" @?=| "(baz,quux)"
    , testCase "Complex leaf query" $
        findHNode exampleTree "foo/bar/quux" @?=| ""
    ]

testTree = testGroup "Content tree tests"
    [ testLeaf
    , testSummary
    , testChildList
    , testTreeProcessing
    , testFindHNode
    ]
