module Tokenizer where

import Data.Char (isSpace)

-- tokenize function utility
isSymbolChar :: Char -> Bool
isSymbolChar x = elem x "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/\\?#<>="

isSymbol :: String -> Bool
isSymbol = all isSymbolChar

isSign :: Char -> Bool
isSign x = elem x "+-"

isNumberChar :: Char -> Bool
isNumberChar x = elem x "0123456789"

isNumber :: String -> Bool
isNumber (x : xs) = ((isSign x) || (isNumberChar x)) && (all isNumberChar xs)

isSeparator :: Char -> Bool
isSeparator '(' = True
isSeparator ')' = True
isSeparator x = isSpace x

-- TOKENIZER
-- convert input string with lisp code into list of tokens
data Token
  = OpenP
  | CloseP
  | Quote
  | Number String
  | Word String
  deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(' : xs) = OpenP : tokenize xs
tokenize (')' : xs) = CloseP : tokenize xs
tokenize ('\'' : xs) = Quote : tokenize xs
tokenize (x : xs)
  | isSign x = tokenizeSigned xs [x]
  | isNumberChar x = tokenizeNumber xs [x]
  | isSymbolChar x = tokenizeWord (x : xs) ""
  | isSeparator x = tokenize xs
  | otherwise = error "unknown symbol in token"

tokenizeSigned :: String -> String -> [Token]
tokenizeSigned [] sign = error "Unary operation without number OR function without arguments"
tokenizeSigned (x : xs) sign
  | isNumberChar x = tokenizeNumber xs (if sign == "+" then [x] else sign ++ [x])
  | isSymbolChar x = tokenizeWord xs (sign ++ [x])
  | isSeparator x = Word sign : tokenize (x : xs)
  | otherwise = error "unknown symbol in token"

tokenizeNumber :: String -> String -> [Token]
tokenizeNumber [] number = Number number : []
tokenizeNumber (x : xs) number
  | isNumberChar x = tokenizeNumber xs (number ++ [x])
  | isSeparator x = Number number : tokenize (x : xs)
  | otherwise = error "unknown symbol in token"

tokenizeWord :: String -> String -> [Token]
tokenizeWord [] word = Word word : []
tokenizeWord (x : xs) word
  | isSymbolChar x = tokenizeWord xs (word ++ [x])
  | isSeparator x = Word word : tokenize (x : xs)
  | otherwise = error "unknown symbol in token"
