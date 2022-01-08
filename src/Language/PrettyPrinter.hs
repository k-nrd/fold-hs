module Language.PrettyPrinter where

import Language.Types as Types

data Iseq
  = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewline

newline :: Char
newline = '\n'

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str
  | newline `elem` str = iConcat $
    case break (== newline) str of
      (l, s') ->
        [ iStr l,
          iNewline,
          case s' of
            "" -> iNil
            _ : s'' -> iStr s''
        ]
  | otherwise = IStr str

iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent = IIndent

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n =
  iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (zipWith (curry layItem) [1 ..] seqs)
  where
    layItem (n, sq) = iConcat [iFWNum 4 n, iStr ") ", iIndent sq, iNewline]

space :: Int -> [Char]
space n = replicate n newline

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, _) : iseqs) = flatten col iseqs
flatten col ((IStr s, _) : iseqs) = s ++ flatten col iseqs
flatten col ((IAppend iseq1 iseq2, indent) : iseqs) = flatten col ((iseq1, indent) : (iseq2, indent) : iseqs)
flatten _ ((INewline, indent) : iseqs) = newline : space indent ++ flatten indent iseqs
flatten col ((IIndent iseq, _) : iseqs) = flatten col ((iseq, col) : iseqs)

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq, 0)]

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend INil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = INil
iInterleave _ [i] = i
iInterleave sep (x : xs) = iAppend (iAppend x sep) (iInterleave sep xs)

printAlternatives :: [Types.Alternative Types.Name] -> Iseq
printAlternatives alts = iConcat 
  [
    iNewline, 
    iInterleave iNewline (map printAlternative alts)
  ]

printAlternative :: Types.Alternative Types.Name -> Iseq
printAlternative (i, vars, expr) = iConcat 
  [
    iStr "<",
    iNum i,
    iStr ">",
    iInterleave (iStr " ") (map iStr vars),
    iStr " -> ",
    printExpression expr,
    iStr ";"
  ]

printExpression :: Types.CoreExpr -> Iseq
printExpression (Types.ENum n) = iStr $ show n
printExpression (Types.EVar v) = iStr v
printExpression (Types.EApp e1 e2) = printExpression e1 `iAppend` iStr " " `iAppend` printAtomicExpression e2
printExpression (Types.ELet isrec defns expr) =
  iConcat
    [ iStr keyword,
      iNewline,
      iStr " ",
      iIndent (printDefinitions defns),
      iNewline,
      iStr "in ",
      printExpression expr
    ]
  where
    keyword = if isrec then "letrec" else "let"
printExpression (Types.ECase p es) =
  iConcat
    [ iStr "case ",
      printAtomicExpression p,
      iStr " of ",
      printAlternatives es
    ]
printExpression (Types.ELam args body) =
  iConcat
    [ iStr "\\",
      iInterleave (iStr " ") (map iStr args),
      iStr " -> ",
      printExpression body
    ]
printExpression (Types.EConstr tag arity) =
  iConcat
    [ iStr "Pack{",
      iStr $ show tag,
      iStr ", ",
      iStr $ show arity
    ]

printDefinitions :: [(Types.Name, Types.CoreExpr)] -> Iseq
printDefinitions defns = iInterleave sep (map printDefinition defns)
  where
    sep = iConcat [iStr ";", iNewline]

printDefinition :: (Types.Name, Types.CoreExpr) -> Iseq
printDefinition (name, expr) = iConcat [iStr name, iStr " = ", iIndent (printExpression expr)]

printAtomicExpression :: Types.CoreExpr -> Iseq
printAtomicExpression e
  | Types.isAtomicExpr e = printExpression e
  | otherwise = iConcat [iStr "(", printExpression e, iStr ")"]

printSupercombinator :: Types.Supercombinator Types.Name -> Iseq
printSupercombinator (name, vars, expr) =
  iConcat
    [ iStr name,
      iStr " ",
      iInterleave (iStr " ") (map iStr vars),
      iStr " = ",
      iIndent (printExpression expr)
    ]

printProgram :: Types.CoreProgram -> Iseq
printProgram prog = iInterleave (iConcat [iStr ";", iNewline]) (map printSupercombinator prog)

prettyPrint :: Types.CoreProgram -> String
prettyPrint = iDisplay . printProgram
