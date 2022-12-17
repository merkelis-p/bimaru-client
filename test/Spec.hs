import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib1 (State(..), Square(..), emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "empty string" $
        renderDocument (DString "") @?= "''"
    , testCase "non-empty string" $
        renderDocument (DString "demo string") @?= "demo string"
     , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
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
gameStartTests = testGroup "Test start document" 
  [    testCase "Game start document" $
          gameStart emptyState gameStartDocument @?= Right gameStartState
      , testCase "Unable to find DMap at the game start" $
          gameStart emptyState (DList []) @?= Left "Unable to find DMap."
      , testCase "Unable to find element with the key 'occupied_rows'" $
          gameStart emptyState gsdWithoutRowKey @?= Left "There's no such element with the key: occupied_rows"
      , testCase "Unable to find row' data" $
          gameStart emptyState gsdWithEmptyRowData @?= Left "Unable to find row' or col' data."
      , testCase "Unable to find 'head' key" $
          gameStart emptyState gsdWithoutHeadKey @?= Left "Key is invalid."
      , testCase "Out of range head DInteger Value" $ 
          gameStart emptyState gsdOutOfRangeHeadValue @?= Left "DInteger x expected for \"head\", 0 < x < 10"
      , testCase "Invalid head value" $ 
          gameStart emptyState gsdHeadInvalidValue @?= Left "DInteger expected for \"head\""
      , testCase "Unable to find 'tail' key" $
          gameStart emptyState gsdWithoutTailKey @?= Left "Key is invalid."
      , testCase "Invalid tail value" $
          gameStart emptyState gsdTailInvalidValue @?= Left "DMap or DNull is expected for \"tail\""
      , testCase "Hint number is invalid" $
          gameStart emptyState gsdWithInvalidHintValue @?= Left "\"number_of_hints\" expected as DInteger"
      , testCase "Hint number is negative value" $
          gameStart emptyState gsdWithInvalidHintNumber @?= Left "\"number_of_hints\" expected to be positive number"
      , testCase "Row data list length not equal 10" $
          gameStart emptyState gsdToLessRowData @?= Left "Expected 10 values for rows and for columns."
  ]

gameStartState :: State 
gameStartState = State {
    rowData = [3,3,0,6,0,2,2,2,0,2],
    colData = [1,1,2,3,1,4,2,4,2,0],
    numHints = 10,
    hintCoords = [],
    board = replicate 100 Water, 
    document = gameStartDocument
}

number_of_hints :: (String,Document)
number_of_hints = ("number_of_hints",DInteger 10)

occupied_cols :: (String,Document)
occupied_cols = ("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])])

occupied_rows :: (String,Document)
occupied_rows = ("occupied_rows",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])])

game_setup_id :: (String,Document)
game_setup_id = ("game_setup_id",DString "d3993d51-041d-4c86-aa69-a6eb85c41e11")

gameStartDocument :: Document 
gameStartDocument = DMap [
            number_of_hints,
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]
gsdWithoutRowKey :: Document 
gsdWithoutRowKey = DMap [
            number_of_hints,
            occupied_cols,
            game_setup_id
            ]
gsdWithEmptyRowData :: Document 
gsdWithEmptyRowData = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap []),
            game_setup_id
            ]

gsdWithoutHeadKey :: Document 
gsdWithoutHeadKey = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("h",DInteger 3),("tail",DMap [("h",DInteger 3),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 6),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdOutOfRangeHeadValue :: Document 
gsdOutOfRangeHeadValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 20),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdHeadInvalidValue :: Document 
gsdHeadInvalidValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DString "string"),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdWithoutTailKey :: Document 
gsdWithoutTailKey = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("t",DMap [("head",DInteger 3),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 6),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 2),("t",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdTailInvalidValue :: Document 
gsdTailInvalidValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("tail",DList [])]),
            game_setup_id
            ]


gsdWithInvalidHintValue :: Document 
gsdWithInvalidHintValue = DMap [
            ("number_of_hints",DString "string"),
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]

gsdWithInvalidHintNumber :: Document 
gsdWithInvalidHintNumber = DMap [
            ("number_of_hints",DInteger (-1)),
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]

gsdToLessRowData :: Document 
gsdToLessRowData = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])]),
            game_setup_id
            ]

hintTests :: TestTree
hintTests = testGroup "Test hint document" [
    testCase "All hints to game start document" $
      hint gameStartState hint_coords @?= Right (hintState gameStartState)
  , testCase "Unable to find DMap" $  
      hint gameStartState (DList[]) @?= Left "Unable to find DMap."
  , testCase "Unable to find coords by key" $
      hint gameStartState hint_coords_without_key @?= Left "There's no such element with the key: coords"
  , testCase "Unable to find DList for hint coords" $
      hint gameStartState hint_coords_without_dlist @?= Left "DList is expexted."
  , testCase "Unable to find DList values for hint coords" $
      hint gameStartState hint_coords_with_empty_dlist @?= Left "DList expected to be not empty."
  , testCase "Unable to find DMap for single coord" $
      hint gameStartState hint_coords_list_without_dmaps @?= Left "Hint coordinates document is invalid."
  , testCase "Couldn't find 'col' key" $
      hint gameStartState hint_coord_without_col_key @?= Left "Key is invalid."
  , testCase "Couldn't find 'row' key" $
      hint gameStartState hint_coord_without_row_key @?= Left "Key is invalid."
  , testCase "Given not DInteger for coord data" $
      hint gameStartState hint_coord_without_DInteger @?= Left "Hint coordinates document is invalid."
  , testCase "Given DInteger for coord data out of range" $
      hint gameStartState hint_coord_number_out_of_range @?= Left "Hint coordinate is out of bounds."
  ]
  
  
-- only for test case "All hints to game start document"
hintState :: State -> State
hintState s = s {
  board = [Water,Water,Water,Water,Ship,Ship,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Ship,Ship,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],
  hintCoords = [(6,0),(5,0),(4,0),(7,7),(7,6),(7,5),(5,3),(6,3),(7,3),(8,3)]
}

hint_coords :: Document
hint_coords = DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coords_without_key :: Document
hint_coords_without_key = DMap [("c",DList [DMap [("col",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coords_without_dlist :: Document
hint_coords_without_dlist = DMap [("coords", DMap [])]

hint_coords_with_empty_dlist :: Document
hint_coords_with_empty_dlist = DMap [("coords", DList [])]

hint_coords_list_without_dmaps :: Document
hint_coords_list_without_dmaps = DMap [("coords", DList [DString "demo"])]

hint_coord_without_col_key :: Document
hint_coord_without_col_key = DMap [("coords",DList [DMap [("c",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_without_row_key :: Document
hint_coord_without_row_key = DMap [("coords",DList [DMap [("col",DInteger 8),("r",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_without_DInteger :: Document
hint_coord_without_DInteger = DMap [("coords",DList [DMap [("col",DString "demo"),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_number_out_of_range :: Document
hint_coord_number_out_of_range = DMap [("coords",DList [DMap [("col",DInteger 20),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]
