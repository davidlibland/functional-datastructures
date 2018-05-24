module LeftistHeap (LeftistHeap) where
import Heap

data LeftistHeap a = Empty | Node a Int (LeftistHeap a) (LeftistHeap a)

makeLeft :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeLeft x l1 l2 = if rank l1 > rank l2
    then Node x (rank l2 + 1) l1 l2
    else Node x (rank l1 + 1) l2 l1

rank :: LeftistHeap a -> Int
rank Empty = 0
rank (Node _ r _ _) = r

instance Heap LeftistHeap where
    empty = Empty
    isEmpty Empty = True
    isEmpty _ = False

    insert x l = merge l $ makeLeft x Empty Empty

    merge Empty l = l
    merge l Empty = l
    merge t1@(Node x i l1 r1) t2@(Node y j l2 r2) =
        if x < y then makeLeft x l1 $ merge r1 t2
        else makeLeft y l2 $ merge r2 t1

    pop Empty = Nothing
    pop (Node x _ l r) = Just (x, merge l r)