module Polyeme.Parser (readExpr, readExprs) where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Char (digitToInt)
import qualified Data.Vector as V
import Polyeme.Datum
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

------------------
-- Atom parsing --
------------------

parseAtom :: Parser Datum
parseAtom =
  parseString
    <|> parseVarargs
    <|> (try parseHashLiteral)
    <|> parseSymbol
    <|> parseNumber

parseVarargs :: Parser Datum
parseVarargs = string ".." >> return (Symbol "..")

parseSymbol :: Parser Datum
parseSymbol = do
  let allowedChars = oneOf "!#$%&|*+-/:<=>?@^_~"
  first <- letter <|> allowedChars
  rest <- many (alphaNum <|> allowedChars)
  return $ Symbol (first : rest)

-------------------
-- Hash literals --
-------------------

parseBool :: Parser Datum
parseBool = do
  bool <- oneOf "tf"
  return $ case bool of
    't' -> Boolean True
    'f' -> Boolean False

parseChar :: Parser Datum
parseChar = do
  char '\''
  chr <- anyChar
  return $ Character chr

parseVec :: Parser Datum
parseVec = do
  char '('
  vec <- sepBy parseExpr spaces
  char ')'
  return $ Vector $ V.fromList vec

parseHashLiteral :: Parser Datum
parseHashLiteral = do
  char '#'
  parseBool <|> parseChar <|> parseVec

parseString :: Parser Datum
parseString = do
  char '"'
  str <- many $ (noneOf "\"") <|> (liftM (!! 1) $ string "\\\"")
  char '"'
  return $ String str

----------------------
-- Numeric literals --
----------------------

parseDecNumber :: Parser Datum
parseDecNumber = do
  integral <- many1 digit
  comma <- optionMaybe (char ',')
  case comma of
    Nothing -> return $ Integer $ read integral
    Just _ -> do
      fractional <- many1 digit
      return $ Real $ read (integral ++ '.' : fractional)

parseBinNumber :: Parser Datum
parseBinNumber = do
  string "0b"
  bin <- many1 (oneOf "01")
  return $ Integer $ foldl (\acc x -> acc * 2 + digitToInt x) 0 bin

parseHexNumber :: Parser Datum
parseHexNumber = do
  string "0x"
  hex <- many1 hexDigit
  return $ Integer $ foldl (\acc x -> acc * 16 + digitToInt x) 0 hex

parseNumber :: Parser Datum
parseNumber = do
  (try parseHexNumber) <|> (try parseBinNumber) <|> parseDecNumber

------------------
-- List parsing --
------------------

parsePair :: Parser Datum
parsePair = do
  list <- sepBy parseExpr spaces
  return $ foldr (\acc x -> Pair acc x) (Symbol "()") list

parseDotted :: Parser Datum
parseDotted = do
  car <- parseExpr
  spaces >> char '.' >> spaces
  cdr <- parseExpr
  return $ Pair car cdr

parseQuoted :: Parser Datum
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ Pair (Symbol "quote") x

parseComment :: Parser ()
parseComment = do
  char ';'
  manyTill anyChar newline
  return ()

parseExpr :: Parser Datum
parseExpr = do
  sepEndBy parseComment (many space)
  parseAtom
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseDotted <|> parsePair
      char ')'
      return x
    <|> do
      char '['
      x <- parsePair
      char ']'
      return x

parseExprs :: Parser [Datum]
parseExprs = do
  endBy parseExpr (parseComment <|> spaces <|> eof)

readExpr :: String -> SourceName -> Result Datum
readExpr input source = liftEither $ left (Parser) $ parse parseExpr source input

readExprs :: String -> SourceName -> Result [Datum]
readExprs input source = liftEither $ left (Parser) $ parse parseExprs source input
