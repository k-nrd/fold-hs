module Language.Types where

type Name = String

type IsRec = Bool

type Alternative a = (Int, [a], Expr a)

type CoreAlternative = Alternative Name

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EApp (Expr a) (Expr a) -- Application
  | ELet IsRec [(a, Expr a)] (Expr a) -- Let(rec) expressions
  | ECase (Expr a) [Alternative a] -- Case expressions
  | ELam [a] (Expr a) -- Lambda expressions
  deriving stock (Show)

type CoreExpr = Expr Name

type Supercombinator a = (Name, [a], Expr a)

type CoreSupercombinator = Supercombinator Name

type Program a = [Supercombinator a]

type CoreProgram = Program Name

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False
