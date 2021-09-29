module Parser where

import qualified Language

type Token = (Int, String)

type Parser a = [Token] -> [(a, [Token])]

whitespaces :: String
whitespaces = " \t"

digits :: String
digits = "0123456789"

doubleOperators :: [String]
doubleOperators = ["==", "!=", "<=", ">=", "->"]

isWhitespace :: Char -> Bool
isWhitespace x = x `elem` whitespaces

isNewline :: Char -> Bool
isNewline x = x == '\n'

isDigit :: Char -> Bool
isDigit x = x `elem` digits

isAlpha :: Char -> Bool
isAlpha x = x `elem` ['a' .. 'z'] ++ ['A' .. 'Z']

isIdentityChar :: Char -> Bool
isIdentityChar x = isDigit x || isAlpha x || x == '_'

isOperator :: Char -> Bool
isOperator x = x `elem` ['=', '<', '>', '!', '-']

isDoubleOperator :: String -> Bool
isDoubleOperator x = x `elem` doubleOperators

lexer :: Int -> String -> [Token]
lexer ln (c : cs) | isNewline c = lexer (ln + 1) cs
lexer ln (c : cs) | isWhitespace c = lexer ln cs
lexer ln (c : cs) | isDigit c = numToken : lexer ln rest
  where
    numToken = (ln, c : takeWhile isDigit cs)
    rest = dropWhile isDigit cs
lexer ln (c : cs) | isAlpha c = varToken : lexer ln rest
  where
    varToken = (ln, c : takeWhile isIdentityChar cs)
    rest = dropWhile isIdentityChar cs
lexer _ [] = undefined
lexer _ [_] = undefined
lexer _ (_ : _ : _) = undefined

syntax :: [Token] -> Language.CoreProgram
syntax = undefined

parse :: String -> Language.CoreProgram
parse = syntax . lexer 0
