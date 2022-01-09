module Language.TemplateInstantiationMachine where

import Data.List (mapAccumL)
import Language.Parser (parse)
import Language.Prelude (preludeDefs)
import Language.Syntax (CoreExpr, CoreProgram, CoreSupercombinator, Name)
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
showResults = undefined

tiStatsIncStep :: TiStats -> TiStats
tiStatsIncStep s = s + 1

tiStatsGetStep :: TiStats -> Int
tiStatsGetStep s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats fn (stack, dump, heap, scDefs, stats) = (stack, dump, heap, scDefs, fn stats)

buildInitialHeap :: [CoreSupercombinator] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSupercombinator initialHeap

allocateSupercombinator :: TiHeap -> CoreSupercombinator -> (TiHeap, (Name, Addr))
allocateSupercombinator heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (SupercombinatorNode name args body)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatsIncStep

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (heapLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NumNode _) = True
isDataNode _ = False

step :: TiState -> TiState
step state = dispatch (heapLookup heap (head stack))
  where
    (stack, _, heap, _, _) = state

    dispatch (NumNode n) = numStep state n
    dispatch (AppNode a1 a2) = appStep state a1 a2
    dispatch (SupercombinatorNode sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as function!"

appStep :: TiState -> Addr -> Addr -> TiState
appStep (stack, dump, heap, globals, stats) a1 _ = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) _ args body = (newStack, dump, newHeap, globals, stats)
  where
    newStack = resultAddr : drop (length args + 1) stack
    (newHeap, resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip args (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc : stack) = map getArg stack
  where
    getArg addr = arg
      where
        (AppNode fn arg) = heapLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate = undefined
