module Language.Prelude where

import Language.Syntax (CoreProgram, Expr (EApp, EVar))

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K2", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EApp (EApp (EVar "f") (EVar "x")) (EApp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EApp (EVar "f") (EApp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EApp (EApp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
