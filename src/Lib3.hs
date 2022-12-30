{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document (..), FromDocument, fromDocument )
import Lib1 (State(..), loadDMap, loadDList, findByKey, getValues, getHintNO, getHints, showHints, removeDuplicates)
import Lib2 (gameDataValidation)
import Control.Applicative ( Alternative((<|>), empty, many) )
import Control.Monad ( replicateM_ )
import Data.Either ()
import Data.Char ( isAlphaNum, isDigit, isAlpha )

---------------------------------------------------------------
-- YAML PARSER
---------------------------------------------------------------

--- Parser type and functions declarations --------------------

newtype Parser a = Parser {
    parser :: String -> Either String (a, String)
    }

instance Functor Parser where
    fmap f (Parser v) = Parser $ \i ->
        case v i of
        Left err -> Left err
        Right (o, xs) -> Right (f o, xs)

instance Applicative Parser where
    Parser f <*> Parser v = Parser $ \i ->
        case f i of
        Left err -> Left err
        Right (f', xs) ->
            case v xs of
            Left err -> Left err
            Right (o, xs') -> Right (f' o, xs')
    pure x = Parser $ \i -> Right (x, i)

instance Monad Parser where
    Parser v >>= w = Parser $ \i ->
        case v i of
        Left err -> Left err
        Right (o, xs) -> let Parser v' = w o in v' xs
    return = pure

instance Alternative Parser where
  Parser x <|> Parser y = Parser $ \i ->
    case x i of
      Right (o, xs) -> Right (o, xs)
      Left err ->
        case y i of
          Left _ -> Left err
          Right (o, xs) -> Right (o, xs)
  empty = Parser $ \_ -> Left "Parser is empty."

charParser :: Char -> Parser Char
charParser c = Parser $ \i ->
    case i of
        (x:xs) | x == c  -> Right (x,xs)
        []               -> Left   "Empty input for charParser."
        x      | x == "" -> Left   "End of input for charParser."
        (x:_)            -> Left $ "Couldn't match input character '" ++ [x] ++ "' with expected '" ++ [c] ++ "' in charParser."

stringParser :: String -> Parser String
stringParser []     = return []
stringParser (x:xs) = charParser x >> stringParser xs >> return (x:xs)

optionalParser :: Parser a -> Parser ()
optionalParser x = () <$ x <|> pure ()

parseWS :: Int -> Parser ()
parseWS n = replicateM_ n (charParser ' ')


---------------------------------------------------------------

-- Parse DString ----------------------------------------------

validateDString :: Parser String
validateDString = Parser $ \i -> let char = takeWhile (\c -> isAlphaNum c || c == ' ' || c == '-') i in
    case char of
        [] -> Left "End of input while parsing non emty string."
        _  -> Right (char, drop (length char) i)

parseNonEmptyDString :: Parser String
parseNonEmptyDString = do
    optionalParser (charParser '"' <|> charParser '\'')
    string <- validateDString
    optionalParser (charParser '"' <|> charParser '\'')
    stringParser "\n"
    return string

parseEmptyDString :: Parser Document
parseEmptyDString = (stringParser "'" >> stringParser "'") <|> stringParser "\"\"" >> stringParser "\n" >> return(DString "")
 

parseDString :: Parser Document
parseDString = DString <$> parseNonEmptyDString <|> parseEmptyDString

---------------------------------------------------------------

-- Parse DInteger ---------------------------------------------

parseNumber :: Parser Int
parseNumber = Parser $ \i -> let digit = takeWhile isDigit i in
    case digit of
        [] -> Left "End of input for parseNumber."
        _  -> Right (read digit, drop (length digit) i)

parseSignedNumber :: Parser Int
parseSignedNumber = do
    optionalParser (charParser '(')
    charParser '-'
    n <- parseNumber
    optionalParser (charParser ')')
    stringParser "\n"
    return (-n)
    <|> do
    n <- parseNumber
    stringParser "\n"
    return n

parseDInteger :: Parser Document
parseDInteger =  DInteger <$> parseSignedNumber

---------------------------------------------------------------

-- Parse DNull  -----------------------------------------------

parseDNull :: Parser Document
parseDNull = DNull <$ stringParser "null\n"

---------------------------------------------------------------

-- Parse DMap -------------------------------------------------

afterKey :: Parser ()
afterKey = () <$ (stringParser ":" >> stringParser " \n" <|> stringParser "\n" <|> stringParser " ")

parseKey :: Parser String
parseKey = Parser $ \i -> let char = takeWhile (\c -> isAlpha c || c == '_' || c == '-') i in
    case char of
        []   -> Left "Key is empty."
        _    -> Right (char, drop (length char) i)

parseEmptyKey :: Parser String
parseEmptyKey  = stringParser "''" >> return ""

parseDMapElement :: Int -> Parser (String, Document)
parseDMapElement n = do
    optionalParser (stringParser "'")
    key <- parseKey <|> parseEmptyKey
    optionalParser (stringParser "'") <* afterKey
    doc <- parseDList n <|> parseDList (n+2) <|> parseDMap (n+2) <|> parseDInteger <|> parseDNull <|> parseDString
    return (key, doc)

parseEmptyDMap :: Parser Document
parseEmptyDMap = stringParser "{}" >> stringParser "\n" >> return(DMap [])

parseDMap :: Int -> Parser Document
parseDMap n = parseEmptyDMap <|> do
    x <- optionalParser (parseWS n) >> parseDMapElement n
    xs <- many (parseWS n >> parseDMapElement n)
    return $ DMap $ x:xs

---------------------------------------------------------------

-- Parse DList ------------------------------------------------

beforeListElement :: Parser ()
beforeListElement = () <$ (stringParser "-" >> stringParser " \n" <|> stringParser "\n" <|> stringParser " ")

parseDListElement :: Int -> Parser Document
parseDListElement n = do
    beforeListElement
    parseDList (n+2) <|> parseDMap (n+2) <|> parseDInteger <|> parseDNull <|> parseDString

parseEmptyDList :: Parser Document
parseEmptyDList = stringParser "[]" >> stringParser "\n" >> return(DList [])

parseDList :: Int -> Parser Document
parseDList n = parseEmptyDList <|> do
    x <- optionalParser (parseWS n) >> parseDListElement n
    xs <- many ( parseWS n  >> parseDListElement n )
    return $ DList $ x:xs


-- Parse a document from yaml --------------------------------

parseDocument :: String -> Either String Document
parseDocument s = fst <$> parser (optionalParser (stringParser "---\n") >> parseDMap 0 <|> parseDList 0 <|> parseDInteger <|> parseDNull <|> parseDString) s

---------------------------------------------------------------
-- GAME START 
---------------------------------------------------------------

data GameStart = GameStart {
      hintNumber :: Int
    , occupiedCols :: [Int]
    , occupiedRows :: [Int]
} deriving Show

instance FromDocument GameStart where
    fromDocument d = do
        initialDMap <- loadDMap d
        occupiedRows' <- findByKey initialDMap "occupied_rows"
        rowData' <- getValues occupiedRows' []
        isRowDataValid <- gameDataValidation rowData'
        occupiedCols' <- findByKey initialDMap "occupied_cols"
        colData' <- getValues occupiedCols' []
        isColDataValid <- gameDataValidation colData'
        numHintsTuple <- findByKey initialDMap "number_of_hints"
        numHints' <- getHintNO numHintsTuple
        Right GameStart {
              hintNumber = numHints'
            , occupiedCols = colData'
            , occupiedRows = rowData'
        }


-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart s d = s {
      rowData = occupiedRows d
    , colData = occupiedCols d
    , numHints = hintNumber d
    }

---------------------------------------------------------------
-- HINTS
---------------------------------------------------------------

data Hint = Hint {
    coordinates :: [(Int,Int)]
} deriving Show

instance FromDocument Hint where
    fromDocument d = do
        dmap <- loadDMap d
        coordsTuple <- findByKey dmap "coords"
        dlist <- loadDList coordsTuple
        hintCoordinates <- getHints dlist []
        Right Hint {
            coordinates = hintCoordinates
        }

hints :: Hint -> [(Int,Int)]
hints = coordinates

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint s h = s {
      board = showHints h' (board s)
    , hintCoords = removeDuplicates (hintCoords s ++ h')
} where h' = hints h
