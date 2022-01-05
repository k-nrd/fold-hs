module Language.Lexer where

import Language.Token

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

lexer :: Int -> String -> [Token]
lexer ln (c : cs)
  | isNewline c = lexer (ln + 1) cs
  | isWhitespace c = lexer ln cs
  | isDigit c = numToken : lexer ln numRest
  | isAlpha c = varToken : lexer ln varRest
  where
    numToken = Token {line = ln, literal = c : takeWhile isDigit cs}
    varToken = Token {line = ln, literal = c : takeWhile isIdentityChar cs}
    numRest = dropWhile isDigit cs
    varRest = dropWhile isIdentityChar cs
lexer ln (c1 : c2 : cs)
  | opLiteral `elem` doubleOperators = opToken : lexer ln rest
  where
    opLiteral = [c1, c2]
    opToken = Token {line = ln, literal = opLiteral}
    rest = cs
lexer ln (c : cs) = tok : lexer ln cs
  where
    tok = Token {line = ln, literal = [c]}
lexer _ [] = []
