module Language.TemplateInstantiationMachine where

import Data.List (mapAccumL)
import Language.Parser (parse)
import Language.Prelude (preludeDefs)
import Language.PrettyPrinter (Iseq, iAppend, iConcat, iDisplay, iIndent, iInterleave, iLayn, iNewline, iNum, iStr, space)
import Language.Syntax (CoreDefinition, CoreExpr, CoreProgram, CoreSupercombinator, Expr (EApp, ECase, EConstr, ELet, ENum, EVar), Name)
import Language.Utils (Addr, Assoc, Heap, addrLookup, heapAlloc, heapLookup, initialHeap)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump

type TiHeap = Heap Node

data Node
  = AppNode Addr Addr
  | SupercombinatorNode Name [Name] CoreExpr
  | NumNode Int

type TiGlobals = Assoc Name Addr

type TiStats = Int

initialTiDump :: TiDump
initialTiDump = DummyTiDump

initialTiStats :: TiStats
initialTiStats = 0

extraPreludeDefs :: [CoreSupercombinator]
extraPreludeDefs = []

runProg :: String -> String
runProg = showResults . eval . compile . parse

compile :: CoreProgram -> TiState
compile program = (initStack, initialTiDump, initHeap, globals, initialTiStats)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initHeap, globals) = buildInitialHeap scDefs
    initStack = [mainAddr]
    mainAddr = addrLookup globals "main" (error "`main` is not defined")

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TiState -> Iseq
showState (stack, _, heap, _, _) = iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack =
  iConcat
    [ iStr "Stack [",
      iNewline,
      iInterleave iNewline (map showStackItem stack),
      iNewline,
      iIndent $ iStr " ]"
    ]
  where
    showStackItem addr =
      iConcat
        [ showFWAddr addr,
          iStr ": ",
          showStackNode heap (heapLookup heap addr)
        ]

showStackNode :: TiHeap -> Node -> Iseq
showStackNode heap (AppNode fnAddr argAddr) =
  iConcat
    [ iStr "AppNode ",
      showFWAddr fnAddr,
      iStr " ",
      showFWAddr argAddr,
      iStr "(",
      showNode (heapLookup heap argAddr),
      iStr ")"
    ]
showStackNode _ node = showNode node

showNode :: Node -> Iseq
showNode (AppNode a1 a2) =
  iConcat
    [ iStr "AppNode ",
      showAddr a1,
      iStr " ",
      showAddr a2
    ]
showNode (SupercombinatorNode name _ _) = iStr ("SupercombinatorNode " ++ name)
showNode (NumNode n) = iStr "NumNode " `iAppend` iNum n

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats :: TiState -> Iseq
showStats (_, _, _, _, stats) =
  iConcat
    [ iNewline,
      iNewline,
      iStr "Total number of steps = ",
      iNum (tiStatsGetSteps stats)
    ]

tiStatsIncSteps :: TiStats -> TiStats
tiStatsIncSteps s = s + 1

tiStatsGetSteps :: TiStats -> Int
tiStatsGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats fn (stack, dump, heap, scDefs, stats) = (stack, dump, heap, scDefs, fn stats)

buildInitialHeap :: [CoreSupercombinator] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSupercombinator initialHeap

allocateSupercombinator :: TiHeap -> CoreSupercombinator -> (TiHeap, (Name, Addr))
allocateSupercombinator heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (SupercombinatorNode name args body)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatsIncSteps

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (heapLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NumNode _) = True
isDataNode _ = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) = dispatch (heapLookup heap (head stack))
  where
    dispatch (NumNode n) = numStep state n
    dispatch (AppNode a1 a2) = appStep state a1 a2
    dispatch (SupercombinatorNode sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as function!"

appStep :: TiState -> Addr -> Addr -> TiState
appStep (stack, dump, heap, globals, stats) a1 _ = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) _ args body =
  (newStack, dump, newHeap, globals, stats)
  where
    newStack = resultAddr : drop (length args + 1) stack
    (newHeap, resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip args (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) =
  map getArg stack
  where
    getArg addr = case heapLookup heap addr of
      AppNode _ arg -> arg
      _ -> error "Can't get arg from non-application node"
getArgs _ _ = error "Can't get args from an empty stack"

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap _ = heapAlloc heap (NumNode n)
instantiate (EApp e1 e2) heap env = heapAlloc heap2 (AppNode a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, addrLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env = instantiateLet isRec defs body heap env
instantiate (ECase _ _) _ _ = error "Can't instantiate case expressions"
instantiate _ _ _ = error "Can't instantiate lambda expressions"

instantiateConstr :: Int -> Int -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr = error "Can't instantiate constructors"

instantiateLet :: Bool -> [CoreDefinition] -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateLet = error "Can't instantiate let(rec) expressions"
