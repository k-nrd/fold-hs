module Language.Parser where

import Language.Lexer (lexer)
import Language.Types 
import Language.Token
import Language.ParserUtils

parse :: String -> CoreProgram
parse = syntax . lexer 0

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . parseProgram
  where 
    takeFirstParse ((prog, []) : _) = prog
    takeFirstParse (_ : others) = takeFirstParse others
    takeFirstParse [] = error "Syntax error" 

parseProgram :: Parser CoreProgram
parseProgram = oneOrMoreWithSep parseSupercombinator (lit ";")

parseSupercombinator :: Parser CoreSupercombinator 
parseSupercombinator = then4 makeSupercombinator ident (zeroOrMore ident) (lit "=") parseExpression
  where
    makeSupercombinator v vs _ expr = (v, vs, expr)

parseExpression :: Parser CoreExpr 
parseExpression = parseLet recursive 
  `ior` parseLet nonRecursive 
  `ior` parseCase
  `ior` parseLambda
  `ior` parseAtomicExpression 
  `ior` parseApplication

parseLet :: IsRec -> Parser CoreExpr 
parseLet isRec = then4 (makeLet isRec) (lit keyword) (oneOrMoreWithSep parseDefinition (lit ";")) (lit "in") parseExpression
  where
    keyword 
      | isRec = "letrec"
      | otherwise = "let"
    makeLet rec _ defns _ = ELet rec defns

parseCase :: Parser CoreExpr
parseCase = then4 
  makeCase 
  (lit "case") 
  parseExpression 
  (lit "of") 
  (oneOrMoreWithSep parseAlternative (lit ";"))
    where makeCase _ expr _ = ECase expr

parseLambda :: Parser CoreExpr
parseLambda = then4 makeLambda (lit "\\") (oneOrMore ident) (lit ".") parseExpression
  where makeLambda _ vs _ = ELam vs 

parseAtomicExpression :: Parser CoreExpr
parseAtomicExpression = parseIdentifier
  `ior` parseNumber
  `ior` parseConstructor 
  `ior` parseParenthesizedExpr

parseIdentifier :: Parser CoreExpr
parseIdentifier = apply ident EVar

parseNumber :: Parser CoreExpr
parseNumber = apply num ENum

parseParenthesizedExpr :: Parser CoreExpr
parseParenthesizedExpr = then3 unwrap (lit "(") parseExpression (lit ")")

parseDefinition :: Parser CoreDefinition
parseDefinition = then3 makeDefinition ident (lit "=") parseExpression
  where
    makeDefinition v _ expr = (v, expr)

parseAlternative :: Parser CoreAlternative 
parseAlternative = then4 makeAlternative parseAltNum (zeroOrMore ident) (lit "->") parseExpression
  where
    makeAlternative i vs _ expr = (i, vs, expr) 

parseAltNum :: Parser Int
parseAltNum = then3 unwrap (lit "<") num (lit ">")

parseConstructor :: Parser CoreExpr
parseConstructor = then3 makeConstructor (lit "Pack{") parseConstructorTags (lit "}")
  where 
    makeConstructor _ (tag, arity) _ = EConstr tag arity
    parseConstructorTags = then3 (\x _ z -> (x, z)) num (lit ",") num

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EApp (EApp (EVar op) e1) e2

parseExpr1 :: Parser CoreExpr
parseExpr1 = then2 assembleOp parseExpr2 parseExpr1c

parseExpr1c :: Parser PartialExpr 
parseExpr1c = then2 FoundOp (lit "|") parseExpr1 `xor` empty NoOp

parseExpr2 :: Parser CoreExpr
parseExpr2 = then2 assembleOp parseExpr3 parseExpr2c

parseExpr2c :: Parser PartialExpr 
parseExpr2c = then2 FoundOp (lit "&") parseExpr2 `xor` empty NoOp

parseExpr3 :: Parser CoreExpr
parseExpr3 = then2 assembleOp parseExpr4 parseExpr3c

parseExpr3c :: Parser PartialExpr 
parseExpr3c = then2 FoundOp (satisfies (`elem` relOps)) parseExpr4 `xor` empty NoOp

parseExpr4 :: Parser CoreExpr
parseExpr4 = then2 assembleOp parseExpr5 parseExpr4c

parseExpr4c :: Parser PartialExpr 
parseExpr4c = 
  (then2 FoundOp (lit "+") parseExpr4 `xor` empty NoOp) `xor` 
  (then2 FoundOp (lit "-") parseExpr5 `xor` empty NoOp)

parseExpr5 :: Parser CoreExpr
parseExpr5 = then2 assembleOp parseApplication parseExpr5c

parseExpr5c :: Parser PartialExpr 
parseExpr5c = 
  (then2 FoundOp (lit "*") parseExpr5 `xor` empty NoOp) `xor` 
  (then2 FoundOp (lit "/") parseApplication `xor` empty NoOp)

parseApplication :: Parser CoreExpr
parseApplication = oneOrMore parseAtomicExpression `apply` makeApplicationChain
  where
    makeApplicationChain (expr1:exprs) = foldl EApp expr1 exprs
    makeApplicationChain _ = error "Can't apply empty list"
