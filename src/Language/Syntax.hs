module Language.Syntax where

type Name = String

type IsRec = Bool

type Program a = [Supercombinator a]

type CoreProgram = Program Name

type Supercombinator a = (Name, [a], Expr a)

type CoreSupercombinator = Supercombinator Name

type Alternative a = (Int, [a], Expr a)

type CoreAlternative = Alternative Name

type Definition a = (a, Expr a)

type CoreDefinition = Definition Name

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EApp (Expr a) (Expr a) -- Application
  | ELet IsRec [Definition a] (Expr a) -- Let(rec) expressions
  | ECase (Expr a) [Alternative a] -- Case expressions
  | ELam [a] (Expr a) -- Lambda expressions
  deriving stock (Show)

type CoreExpr = Expr Name

data PartialExpr
  = NoOp
  | FoundOp Name CoreExpr

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

recursive :: IsRec
recursive = True

nonRecursive :: IsRec
nonRecursive = False
