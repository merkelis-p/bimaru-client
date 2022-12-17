{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE InstanceSigs #-}
module Lib1(
    State(..), Square(..), emptyState, gameStart, hint, render, mkCheck, toggle, loadDMap, loadDList, getValues, getHintNO, findByKey, getHints, showHints, removeDuplicates
) where


import Prelude
import Types
import Data.Either

-- Gets DMap value -> [(String, Document)]
loadDMap :: Document -> Either String [(String, Document)]
loadDMap (DMap xs) = Right xs
loadDMap _ = Left "Unable to find DMap."


-- Gets DList value -> [Document]
loadDList :: Document -> Either String [Document]
loadDList (DList []) = Left "DList expected to be not empty."
loadDList (DList xs) = Right xs
loadDList _ = Left "DList is expexted."

-- Checks whether keys are the same
keyValidation :: String -> String -> Either String String
keyValidation k k' = if k /= k' then Left "Key is invalid." else Right "Key is valid."

-- Gets a DMap element by key
findByKey :: [(String, Document)] -> String -> Either String Document
findByKey [] k = Left ("There's no such element with the key: " ++ k)
findByKey ((s, d):xs) k = if k == s then Right d else findByKey xs k

---------------------------------------------------------------------------------------------------------------------------------------
-- SHOW
---------------------------------------------------------------------------------------------------------------------------------------

data Square = Water
            | Ship deriving Eq
instance Show Square where
    show :: Square -> String
    show Water =  "≈"
    show Ship  =  "x"

-- State definition 
data State = State {
    rowData :: [Int],
    colData :: [Int],
    numHints :: Int,
    hintCoords :: [(Int,Int)],
    board :: [Square],
    document :: Document
} deriving (Eq, Show)

-- Empty state definition 
emptyState :: State
emptyState = State {rowData = [], colData = [], numHints = 0, hintCoords = [], board = replicate 100 Water, document = DNull}

-- Gets a row' and col' data 
getValues :: Document -> [Int] -> Either String [Int]
getValues DNull ns = Right ns
getValues d ns = do
    ht <- loadDMap' d
    n <- isHead (head ht)
    document <- isTail (ht !! 1)
    getValues document (ns ++ [n])

loadDMap' :: Document -> Either String [(String, Document)]
loadDMap' (DMap []) = Left "Unable to find row' or col' data."
loadDMap' (DMap l) = Right l
loadDMap' _ = Left "Unable to find row' or col' data."

isHead :: (String, Document) -> Either String Int
isHead (k, d) = do
    key <- keyValidation k "head"
    headValidation d

headValidation :: Document -> Either String Int
headValidation (DInteger x) = do
    if (x < 0) || (x > 10)
        then Left "DInteger x expected for \"head\", 0 < x < 10"
        else Right x
headValidation _ = Left "DInteger expected for \"head\""

isTail :: (String, Document) -> Either String Document
isTail (k, d) = do
    key <- keyValidation k "tail"
    tailValidation d

tailValidation :: Document -> Either String Document
tailValidation DNull = Right DNull
tailValidation (DMap ht) = Right (DMap ht)
tailValidation _ = Left "DMap or DNull is expected for \"tail\""

getHintNO :: Document -> Either String Int
getHintNO (DInteger x) = if x >= 0 then Right x else Left "\"number_of_hints\" expected to be positive number"
getHintNO _ = Left "\"number_of_hints\" expected as DInteger"

-- Adds game data to initial state 
gameStart :: State -> Document -> State
gameStart s d = s {
    rowData = fromRight [] (
    getValues (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "occupied_rows")) []),
    colData = fromRight [] ( getValues (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "occupied_cols")) []),
    document = d,
    numHints = fromRight 0 (getHintNO (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "number_of_hints")))
}

-- IMPLEMENT
-- renders your game board
splitBoard :: [a] -> [[a]]
splitBoard [] = []
splitBoard xs = take 10 xs : splitBoard (drop 10 xs)


render :: State -> String
render s = do
    concat ([show (board s)] ++ "\n\nNumber of hints left: " : show (numHints s - length (hintCoords s)) : ["\n\n    1  2  3  4  5  6  7  8  9  10 \n"] ++ rows ++ ["   "] ++ map (\x -> " " ++ show x ++ " ") (colData s) ++ ["\n"])
    where
        rows' = map (\ys -> " " ++ concatMap (\y -> show y ++ "  ") ys) (splitBoard (board s))
        rows  = [(if i < 9 then " " else "") ++ show (i + 1) ++ " " ++ (rows' !! i) ++ "| " ++ show (rowData s !! i) ++ "\n" | i <- [0..9]] ++ ["    ————————————————————————————\n"]

---------------------------------------------------------------------------------------------------------------------------------------
-- CHECK
---------------------------------------------------------------------------------------------------------------------------------------

-- Gets a list of indexes of a given list elements that is true with the function f
getIDIf :: Int -> (a -> Bool) -> [a] -> [Int]
getIDIf _ _ [] = []
getIDIf i f (x:xs) = if f x then i : getIDIf (i + 1) f xs else getIDIf (i + 1) f xs

-- Creates a list of Coordinates of 10 x 10 game board
createCoordList :: [Square] -> [(Int,Int)]
createCoordList xs = zip l r
     where xs' = getIDIf 0 (== Ship) xs
           l = map (`mod` 10) xs'
           r = map (`div` 10) xs'

-- Creates a list of Coord
toCoord :: [(Int, Int)] -> [Coord]
toCoord [] = []
toCoord ((x,y):xs) = Coord x y : toCoord xs

-- Makes check from current state
mkCheck :: State -> Check
mkCheck s = Check (toCoord(createCoordList(board s)))

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
---------------------------------------------------------------------------------------------------------------------------------------
-- TOGGLE
---------------------------------------------------------------------------------------------------------------------------------------

-- Changes nth element in a list with a given value
changeElementAt :: [a] -> a -> Int -> [a]
changeElementAt b sq n = xs ++ [sq] ++ drop 1 ys
    where (xs, ys) = splitAt n b

-- Toggle state's board value
-- Does not validates user input
toggle :: State -> [String] -> State
toggle s t = s {board = board'}
    where
        tc = read (head t)
        tr = read (t !! 1)
        i = (tc - 1) + (tr - 1) * 10
        b = board s
        square' = if b !! i == Water then Ship else Water
        board' = changeElementAt b square' i

---------------------------------------------------------------------------------------------------------------------------------------
-- HINT
---------------------------------------------------------------------------------------------------------------------------------------

-- Shows hints on board :: [Square]
showHints :: [(Int, Int)] -> [Square] -> [Square]
showHints [] board  = board
showHints ((x,y):xs) board = showHints xs board'
    where
        board' = changeElementAt board Ship (x + y * 10)

-- Get list of tuples of hint coordinates 
getHints :: [Document] -> [(Int, Int)] -> Either String [(Int, Int)]
getHints [] ys = Right ys
getHints (x:xs) ys = do
    numbers <- getHintCoords x
    getHints xs (numbers : ys)


-- Gets a tuple of hint coordinates
getHintCoords :: Document -> Either String (Int, Int)
getHintCoords (DMap [(kx,DInteger x),(ky,DInteger y)]) = do
    isCol <- keyValidation kx "col"
    isRow <- keyValidation ky "row"
    if (x < 0) || (x > 9) || (y < 0) || (y > 9) then Left "Hint coordinate is out of bounds." else Right (x, y)
getHintCoords _ = Left "Hint coordinates document is invalid."

-- IMPLEMENT
-- Removes duplicates from a given list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Adds hint data to the game state
hint :: State -> Document -> State
hint s h = s {
    board = showHints hintCoordinates (board s),
    hintCoords = removeDuplicates(hintCoordinates ++ hintCoords s)
} where hintCoordinates = fromRight [] (getHints (fromRight [] ( loadDList ( fromRight DNull ( findByKey (fromRight [] (loadDMap h)) "coords" ) ) ) ) [])

