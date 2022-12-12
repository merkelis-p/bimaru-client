{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE InstanceSigs #-}
module Lib1(
    State(..), Square(..), emptyState, gameStart, render, mkCheck, toggle, hint, 
) where

import Prelude
import Types
import Data.Either

data Square = Water
            | Ship deriving Eq
instance Show Square where
    show :: Square -> String
    show Water =  "≈"
    show Ship  =  "x"

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
    rowData :: [Int],
    colData :: [Int],
    numHints :: Int,
    board :: [Square],
    document :: Document
}
-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {rowData = [], colData = [], numHints = 0, board = replicate 100 Water, document = DNull}

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart s d = s {
    rowData = fromRight [] (
    getValues (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "occupied_rows")) []),
    colData = fromRight [] ( getValues (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "occupied_cols")) []),
    document = d,
    numHints = fromRight 0 (loadHintNO (fromRight DNull (findByKey (fromRight [] (loadDMap d)) "number_of_hints")))
}

-- Gets DMap value -> [(String, Document)]
loadDMap :: Document -> Either String [(String, Document)]
loadDMap (DMap xs) = Right xs
loadDMap _ = Left "Unable to find DMap."


-- Gets DList value -> [Document]
loadDList :: Document -> Either String [Document]
loadDList (DList xs) = Right xs
loadDList _ = Left "DList is expexted."

-- Checks whether keys are the same
keyValidation :: String -> String -> Either String String
keyValidation k k' = if k /= k' then Left "Wrong" else Right "Good"

-- Gets a DMap element by key
findByKey :: [(String, Document)] -> String -> Either String Document
findByKey [] k = Left ("There's no such element with the key: \"" ++ k)
findByKey ((s, d):xs) k = if k == s then Right d else findByKey xs k

-- Gets a row' and col' data 
getValues :: Document -> [Int] -> Either String [Int]
getValues DNull ns = Right ns
getValues d ns = do
    ht <- getHead d
    n <- isHead (head ht)
    document <- isTail (ht !! 1)
    getValues document (ns ++ [n])

getHead :: Document -> Either String [(String, Document)]
getHead (DMap l) = Right l
getHead _ = Left "row' and col' data expected"

isHead :: (String, Document) -> Either String Int
isHead (k, d) = do
    key <- keyValidation k "head"
    headValidation d

headValidation :: Document -> Either String Int
headValidation (DInteger x) = do
    if (x < 0) || (x > 10)
        then Left "DInteger x expected for \"head\", 0 < x < 10 "
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

loadHintNO :: Document -> Either String Int
loadHintNO (DInteger x) = Right x
loadHintNO _ = Left "\"number_of_hints\" expected as DInteger"

-- IMPLEMENT
-- renders your game board
splitBoard :: [a] -> [[a]]
splitBoard [] = []
splitBoard xs = take 10 xs : splitBoard (drop 10 xs)

render :: State -> String
render s = do
    concat ((if numHints s > 0 then "Number of hints left: " : [show (numHints s)] else ["You're out of hints!"]) ++ ["\n\n    1  2  3  4  5  6  7  8  9  10 \n"] ++ rows ++ ["   "] ++ map (\x -> " " ++ show x ++ " ") (colData s))
    where
        rows' = map (\ys -> " " ++ concatMap (\y -> show y ++ "  ") ys) (splitBoard (board s))
        rows  = [(if i < 9 then " " else "") ++ show (i + 1) ++ " " ++ (rows' !! i) ++ "| " ++ show (rowData s !! i) ++ "\n" | i <- [0..9]] ++ ["    ————————————————————————————\n"]

-- IMPLEMENT
-- Make check from current state
coordIndex :: Int -> (a -> Bool) -> [a] -> [Int]
coordIndex _ _ [] = []
coordIndex i f (x:xs) = if f x then i : coordIndex (i + 1) f xs else coordIndex (i + 1) f xs

createCoordList :: [Square] -> [(Int,Int)]
createCoordList xs = zip l r
     where xs' = coordIndex 0 (== Ship) xs
           l = map (`mod` 10) xs'
           r = map (`div` 10) xs'

toCoord :: [(Int, Int)] -> [Coord]
toCoord [] = []
toCoord ((x,y):xs) = Coord x y : toCoord xs

mkCheck :: State -> Check
mkCheck state = Check (toCoord (createCoordList (board state)))

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
changeElementAt :: [a] -> a -> Int -> [a]
changeElementAt b sq n = xs ++ [sq] ++ drop 1 ys
    where (xs, ys) = splitAt n b

toggle :: State -> [String] -> State
toggle s t = s {board = board'}
    where
        tc = read (head t)
        tr = read (t !! 1)
        i = (tc - 1) + (tr - 1) * 10
        b = board s
        square' = if b !! i == Water then Ship else Water
        board' = changeElementAt b square' i

showHints :: [(Int, Int)] -> [Square] -> [Square]
showHints [] board  = board
showHints ((x,y):xs) board = showHints xs board'
    where
        board' = changeElementAt board Ship (x + y * 10)

getHints :: [Document] -> [(Int, Int)] -> Either String [(Int, Int)]
getHints [] ys = Right ys
getHints (x:xs) ys = do
    numbers <- getHintCoords x
    getHints xs (numbers : ys)


getHintCoords :: Document -> Either String (Int, Int)
getHintCoords (DMap [(kx,DInteger x),(ky,DInteger y)]) = do
    isCol <- keyValidation kx "col"
    isRow <- keyValidation ky "row"
    if (x < 0) || (x > 9) || (y < 0) || (y > 9) then Left "Hint coordinate is out of bounds" else Right (x, y)

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint s h = s {
    board = showHints (fromRight [] ( getHints (fromRight [] ( loadDList ( fromRight DNull ( findByKey (fromRight [] (loadDMap h)) "coords" ) ) ) ) [] ) ) (board s),
    numHints = if numHints s > 0 then numHints s - length (fromRight [] (getHints (fromRight [] ( loadDList ( fromRight DNull ( findByKey (fromRight [] (loadDMap h)) "coords" ) ) ) ) [])) else 0
} 

