module Language.ParserUtils where
  
import Language.Token
import Language.Lexer (isAlpha, isDigit)

type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

relOps :: [String]
relOps = ["<", "<=", ">", ">=", "==", "!="]

unwrap :: a -> b -> c -> b
unwrap _ b _ = b

lit :: String -> Parser String
lit s = satisfies (== s)

ident :: Parser String
ident = satisfies kwpred
  where
    kwpred s
      | s `elem` keywords = False
      | all isAlpha s = True
      | otherwise = False

num :: Parser Int
num = apply (satisfies (all isDigit)) read

ior :: Parser a -> Parser a -> Parser a
ior p1 p2 toks = p1 toks ++ p2 toks

xor :: Parser a -> Parser a -> Parser a
xor p1 p2 toks
  | null res1 = p2 toks
  | otherwise = res1
  where
    res1 = p1 toks

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
zeroOrMore p = oneOrMore p `xor` empty []

satisfies :: (String -> Bool) -> Parser String
satisfies predicate (Token (_, literal) : toks)
  | predicate literal = [(literal, toks)]
  | otherwise = []
satisfies _ [] = []

apply :: Parser a -> (a -> b) -> Parser b
apply p f toks = [(f v, ts) | (v, ts) <- p toks]

oneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
oneOrMoreWithSep p psep = then2 (:) p (zeroOrMore (then2 (\_ y -> y) psep p))
