module Language.Parser where

import Language.Lexer (isAlpha, isDigit, lexer)
import Language.Token
import Language.Types (CoreProgram)

type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

parseLiteral :: String -> Parser String
parseLiteral s = satisfies (== s)

parseVariable :: Parser String
parseVariable = satisfies kwpred
  where
    kwpred s
      | s `elem` keywords = False
      | all isAlpha s = True
      | otherwise = False

parseNum :: Parser Int
parseNum = apply (satisfies (all isDigit)) read

alt :: Parser a -> Parser a -> Parser a
alt p1 p2 toks = p1 toks ++ p2 toks

then2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
then2 combine p1 p2 toks =
  [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1
  ]

then3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
then3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2
  ]

then4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
then4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2,
      (v4, toks4) <- p4 toks3
  ]

empty :: a -> Parser a
empty v toks = [(v, toks)]

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = then2 (:) p (zeroOrMore p)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p `alt` empty []

satisfies :: (String -> Bool) -> Parser String
satisfies predicate (tok : toks)
  | predicate (literal tok) = [(literal tok, toks)]
  | otherwise = []
satisfies _ [] = []

apply :: Parser a -> (a -> b) -> Parser b
apply p f toks = [(f v, ts) | (v, ts) <- p toks]

oneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
oneOrMoreWithSep p psep = then2 (:) p (zeroOrMore (then2 (\_ y -> y) psep p))

syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . lexer 0
