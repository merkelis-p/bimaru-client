{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State

-- IMPLEMENT
-- renders your game board
render :: State -> String

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
