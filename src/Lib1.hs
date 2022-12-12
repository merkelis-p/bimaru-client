{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE InstanceSigs #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
    State(..), Square(..), emptyState, gameStart, render, mkCheck, toggle, hint,
) where

import Prelude
import Types
    ( Check(Check),
      Document(DInteger, DList, DNull, DMap),
      Coord(Coord) )

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State [String]
    deriving Show
-- Gets DMap value -> [(String, Document)]
loadDMap :: Document -> [(String, Document)]
loadDMap (DMap xs) = xs
loadDMap _ = []

-- IMPLEMENT
-- This is very initial state of your program
-- Gets DList value -> [Document]
loadDList :: Document -> [Document]
loadDList (DList xs) = xs
loadDList _ = []

-- Gets a Document element from a list of tuples (Document <- [(String,Document)]) if finds same String (key)
findByKey :: [(String, Document)] -> String -> Document
findByKey [] _ = DNull
findByKey ((k, d):xs) k' = if k == k' then d else findByKey xs k'


---------------------------------------------------------------------------------------------------------------------------------------
-- SHOW
---------------------------------------------------------------------------------------------------------------------------------------

-- Defines one Square in a board
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
    hintNumber :: Int,
    hintCoords :: [(Int,Int)],
    board :: [Square],
    document :: Document
}

-- Empty state definition 
emptyState :: State
emptyState = State ["Initial state"]
emptyState = State {rowData = [], colData = [], hintNumber = 0, hintCoords = [], board = replicate 100 Water, document = DNull}

-- Converts DInteger to Int
getDInteger :: Document -> Int
getDInteger (DInteger x) = x
getDInteger _ = -1

-- Gets row' and col' data 
getValues :: Document -> [Int] -> [Int]
getValues DNull ns = ns
getValues d ns = getValues d' (ns ++ [n])
    where
        ht = loadDMap d
        n = getDInteger(findByKey ht "head")
        d' = findByKey ht "tail"

-- IMPLEMENT
-- This adds game data to initial state 
-- Adds game data to initial state 
gameStart :: State -> Document -> State
gameStart s d = s {
    rowData = getValues (findByKey (loadDMap d) "occupied_rows") [],
    colData = getValues (findByKey (loadDMap d) "occupied_cols") [],
    document = d,
    hintNumber = getDInteger (findByKey (loadDMap d) "number_of_hints")
}

-- IMPLEMENT
-- renders your game board
-- Splits list' every 10th element to a list of list
splitBoard :: [a] -> [[a]]
splitBoard [] = []
splitBoard xs = take 10 xs : splitBoard (drop 10 xs)

-- Renders game board
render :: State -> String
render = show
render s = do
    concat ("\nNumber of hints left: " : show (hintNumber s - length (hintCoords s)) : "\nTo get next hint use \"hint ": [show(length (hintCoords s) +1)] ++ ["\" command\n\n    1  2  3  4  5  6  7  8  9  10 \n"] ++ rows ++ ["   "] ++ map (\x -> " " ++ show x ++ " ") (colData s) ++ ["\n"])
    where
        rows' = map (\ys -> " " ++ concatMap (\y -> show y ++ "  ") ys) (splitBoard (board s))
        rows  = [(if i < 9 then " " else "") ++ show (i + 1) ++ " " ++ (rows' !! i) ++ "| " ++ show (rowData s !! i) ++ "\n" | i <- [0..9]] ++ ["    ————————————————————————————\n"]

---------------------------------------------------------------------------------------------------------------------------------------
-- CHECK
---------------------------------------------------------------------------------------------------------------------------------------

-- IMPLEMENT
-- Make check from current state
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

-- Makes a check from a current state
mkCheck :: State -> Check
mkCheck _ = Check []
mkCheck s = Check (toCoord (createCoordList (board s)))

---------------------------------------------------------------------------------------------------------------------------------------
-- TOGGLE
---------------------------------------------------------------------------------------------------------------------------------------

-- Changes nth element in a list with a given value
changeElementAt :: [a] -> a -> Int -> [a]
changeElementAt b sq n = xs ++ [sq] ++ drop 1 ys
    where (xs, ys) = splitAt n b

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
-- Toggle state's board 
-- Does not validates user input
toggle :: State -> [String] -> State
toggle (State l) t = State $ ("Toggle " ++ show t) : l
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
getHints :: [Document] -> [(Int, Int)] -> [(Int, Int)]
getHints [] ys = ys
getHints (x:xs) ys = getHints xs (numbers : ys)
    where
        numbers = getHintCoords x

-- Gets a tuple of hint coordinates
getHintCoords :: Document -> (Int, Int)
getHintCoords (DMap [("col",DInteger x),("row",DInteger y)]) = (x,y)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State l) h = State $ ("Hint " ++ show h) : lhint s h = s {
    board = showHints hintCoordinates (board s),
    hintCoords = removeDuplicates(hintCoordinates ++ hintCoords s)
} where hintCoordinates = getHints ( loadDList ( findByKey ( loadDMap h) "coords" ) ) []
