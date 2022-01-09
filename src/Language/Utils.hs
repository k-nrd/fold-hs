module Language.Utils where

type Assoc a b = [(a, b)]

type Addr = Int

type Heap a = (Int, [Addr], Assoc Addr a)

showAddr :: Addr -> String
showAddr addr = "#" ++ show addr

addrLookup :: (Eq a) => Assoc a b -> a -> b -> b
addrLookup ((key', val) : bs) key def
  | key == key' = val
  | otherwise = addrLookup bs key def
addrLookup [] _ def = def

initialHeap :: Heap a
initialHeap = (0, [1 ..], [])

heapAlloc :: Heap a -> a -> (Heap a, Addr)
heapAlloc (size, next : free, cts) n = ((size + 1, free, (next, n) : cts), next)
heapAlloc (_, [], _) n = heapAlloc initialHeap n

heapLookup :: Heap a -> Addr -> a
heapLookup (_, _, cts) addr = addrLookup cts addr (error $ "Can't find node " ++ showAddr addr ++ " in heap")
