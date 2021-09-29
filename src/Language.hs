module Language where

type Name = String

type IsRec = Bool

type Alter a = (Int, [a], Expr a)

type CoreAlt = Alter Name

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Application
  | ELet IsRec [(a, Expr a)] (Expr a) -- Let(rec) expressions
  | ECase (Expr a) [Alter a] -- Case expressions
  | ELam [a] (Expr a) -- Lambda expressions
  deriving stock (Show)

type CoreExpr = Expr Name

type ScDefn a = (Name, [a], Expr a)

type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]

type CoreProgram = Program Name

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K2", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
