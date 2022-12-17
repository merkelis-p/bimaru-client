{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document, Check )
import Lib1 (State(..))
import Types ( 
    ToDocument(..), 
    Document (DMap, DList, DInteger, DNull, DString), 
    Check (coords), Coord (col, row) 
    )
import Lib1 (State(..), loadDMap, loadDList, findByKey, getValues, getHintNO, getHints, showHints, removeDuplicates)

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check
    where
    toDocument coords' = DMap [("coords", DList (map (\c -> DMap [("col", DInteger (col c)), ("row", DInteger (row c))]) (coords coords')))]
    
addWS :: Int -> String
addWS i = take i $ repeat ' '

toYamlSingleDocument :: Document -> String
toYamlSingleDocument DNull = "null"
toYamlSingleDocument (DInteger n) = show n
toYamlSingleDocument (DString "") = "''"
toYamlSingleDocument (DString s) = s

toYamlDMap :: Int -> Document -> [String]
toYamlDMap i d =
            case d of
            (DMap []) -> []
            (DMap ((k, DMap []) : xs))   -> (prefix i k  ++ "[]") : toYamlDMap i (DMap xs)
            (DMap ((k, DMap v) : xs))    -> prefix i k : (toYamlDMap (i + 2) (DMap v) ++ toYamlDMap i (DMap xs))
            (DMap ((k,  DList []) : xs)) -> (prefix i k ++ "[]") : toYamlDMap i (DMap xs)
            (DMap ((k,  DList v) : xs))  -> prefix i k : (tomYamlDList (i + 2) (DList v) ++ toYamlDMap i (DMap xs))
            (DMap ((k, v) : xs))         -> (prefix i k ++ toYamlSingleDocument v) : (toYamlDMap i (DMap xs))
            where prefix :: Int -> String -> String 
                  prefix i' k' = (addWS i') ++ (if (k' == "") then "''" else k') ++ ": "

tomYamlDList :: Int -> Document -> [String]
tomYamlDList i d = 
            case d of 
            (DList []) -> []
            (DList ((DList []) : xs))  -> ((addWS i ++ "- []") : tomYamlDList i (DList xs))
            (DList ((DList x) : xs))   -> ((addWS i ++ "- "  ) : tomYamlDList (i + 2) (DList x)) ++ tomYamlDList i (DList xs) 
            (DList ((DMap []) : xs))   -> ((addWS i ++ "- []") : tomYamlDList i (DList xs))
            (DList ((DMap  x) : xs))   -> ((addWS i ++ "- "  ) : toYamlDMap (i + 2) (DMap x)) ++ tomYamlDList i (DList xs)
            (DList (x : xs))           -> ( addWS i ++ "- " ++ toYamlSingleDocument x)  : (tomYamlDList i (DList xs)) 

-- Renders document to yaml
renderDocument :: Document -> String 
renderDocument (DMap [])   = "---\n[]"
renderDocument (DList [])  = "---\n[]"
renderDocument (DList xs)   = "---\n" ++ unlines (tomYamlDList 0 (DList xs)) 
renderDocument (DMap  xs)   = "---\n" ++ unlines (toYamlDMap 0 (DMap  xs)) 
renderDocument d = toYamlSingleDocument d

gameDataValidation :: [Int] -> Either String String
gameDataValidation xs = if length xs /= 10 then Left "Expected 10 values for rows and for columns." else Right "Game values are valid."

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart s d = do
    initialDMap <- loadDMap d
    occupiedRows <- findByKey initialDMap "occupied_rows"
    rowData' <- getValues occupiedRows []
    isRowDataValid <- gameDataValidation rowData'
    occupiedCols <- findByKey initialDMap "occupied_cols" 
    colData' <- getValues occupiedCols []
    isColDataValid <- gameDataValidation colData'
    numHintsTuple <- findByKey initialDMap "number_of_hints"
    numHints' <- getHintNO numHintsTuple
    Right s {
        rowData = rowData',
        colData = colData',
        document = d,
        numHints = numHints'
    }

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint s h = do
    dmap <- loadDMap h
    coordsTuple <- findByKey dmap "coords"
    dlist <- loadDList coordsTuple
    hintCoordinates <- getHints dlist []
    Right s {
        board = showHints hintCoordinates (board s),
        hintCoords = removeDuplicates (hintCoordinates ++ hintCoords s)
    } 
    
