module PairingHeap (PairingHeap(..)) where
import Heap

data PairingHeap a = E | T a [PairingHeap a] deriving Show

instance Heap PairingHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    merge E h = h
    merge h E = h
    merge h1@(T x h1s) h2@(T y h2s) = if x < y
        then T x (h2:h1s)
        else T y (h1:h2s)

    insert x = merge (T x [])

    pop E = Nothing
    pop (T a hs) = Just (a, h)
        where
            h = mergeList hs
            mergeList [] = E
            mergeList [h] = h
            mergeList (h1:h2:hss) = let h12 = merge h1 h2
                in merge h12 $ mergeList hss
