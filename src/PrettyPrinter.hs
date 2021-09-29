module PrettyPrinter where

import qualified Language

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

printAlternatives :: [Language.Alter Language.Name] -> Iseq
printAlternatives = iConcat . map printAlternative

printAlternative :: Language.Alter Language.Name -> Iseq
printAlternative _ = iStr "alt" -- need to improve here

printExpression :: Language.CoreExpr -> Iseq
printExpression (Language.ENum n) = iStr $ show n
printExpression (Language.EVar v) = iStr v
printExpression (Language.EAp e1 e2) = printExpression e1 `iAppend` iStr " " `iAppend` printAtomicExpression e2
printExpression (Language.ELet isrec defns expr) =
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
printExpression (Language.ECase p es) =
  iConcat
    [ iStr "case ",
      printExpression p,
      iStr "of ",
      printAlternatives es
    ]
printExpression (Language.ELam args body) =
  iConcat
    [ iStr "\\",
      iInterleave (iStr " ") (map iStr args),
      iStr " -> ",
      printExpression body
    ]
printExpression (Language.EConstr tag arity) =
  iConcat
    [ iStr "Pack{",
      iStr $ show tag,
      iStr ", ",
      iStr $ show arity
    ]

printDefinitions :: [(Language.Name, Language.CoreExpr)] -> Iseq
printDefinitions defns = iInterleave sep (map printDefinition defns)
  where
    sep = iConcat [iStr ";", iNewline]

printDefinition :: (Language.Name, Language.CoreExpr) -> Iseq
printDefinition (name, expr) = iConcat [iStr name, iStr " = ", iIndent (printExpression expr)]

printAtomicExpression :: Language.CoreExpr -> Iseq
printAtomicExpression e
  | Language.isAtomicExpr e = printExpression e
  | otherwise = iStr "(" `iAppend` printExpression e `iAppend` iStr ")"

printSupercombinatorDefinition :: Language.ScDefn Language.Name -> Iseq
printSupercombinatorDefinition (name, vars, expr) =
  iConcat
    [ iStr name,
      iStr " ",
      iInterleave (iStr " ") (map iStr vars),
      iStr " = ",
      iIndent (printExpression expr)
    ]

printProgram :: Language.CoreProgram -> Iseq
printProgram prog = iInterleave (iConcat [iStr ";", iNewline]) (map printSupercombinatorDefinition prog)

prettyPrint :: Language.CoreProgram -> String
prettyPrint = iDisplay . printProgram
