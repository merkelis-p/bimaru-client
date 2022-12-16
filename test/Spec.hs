import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
    , testCase "empty string" $
        renderDocument (DString "") @?= "''"
    , testCase "non-empty string" $
        renderDocument (DString "demo string") @?= "demo string"
     , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
     , testCase "list of labeled ints" $
        renderDocument (DMap [("demo",DInteger 5),("demo1",DInteger 6),("demo2",DInteger 7)]) @?= listOfLabeledInts
     , testCase "list of nested labeled ints" $
        renderDocument (DMap [("head",DInteger 5),("tail",DMap[("head",DInteger 6),("tail",DMap([("head",DInteger 7),("tail",DNull)]))])]) @?= listOfLabeledNestedInts
     , testCase "list of coordinates" $
        renderDocument (DList [DMap [("x",DInteger 5),("y",DInteger 6)],DMap [("x",DInteger 3),("y",DInteger 2)],DMap [("x",DInteger 4),("y",DInteger 5)]]) @?= listOfCoordinates
     , testCase "labeled list of ints" $
        renderDocument (DMap [("ints",DList[DInteger 5, DInteger 6, DInteger 7])]) @?= labeledListOfInts
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]
listOfLabeledInts :: String
listOfLabeledInts = unlines [
      "---"
    , "demo: 5"
    , "demo1: 6"
    , "demo2: 7"
  ]
listOfLabeledNestedInts :: String
listOfLabeledNestedInts = unlines [
      "---"
    , "head: 5"
    , "tail: " 
    , "  head: 6"
    , "  tail: " 
    , "    head: 7"
    , "    tail: null" 
  ]

listOfCoordinates :: String
listOfCoordinates = unlines [
      "---"
    , "- "  
    , "  x: 5"
    , "  y: 6"
    , "- "
    , "  x: 3" 
    , "  y: 2"
    , "- "
    , "  x: 4"
    , "  y: 5"
  ]
labeledListOfInts :: String
labeledListOfInts = unlines [
      "---"
    , "ints: "
    , "  - 5"
    , "  - 6"
    , "  - 7"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []