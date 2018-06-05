module BootstrapHeap (BootstrapHeap(..)) where
import Heap

data BootstrapHeap h a = E | H a (h (BootstrapHeap h a))
instance Eq a => Eq (BootstrapHeap h a) where
    (H x _) == (H y _) = (x == y)
instance Ord a => Ord (BootstrapHeap h a) where
    (H x _) <= (H y _) = (x <= y)

instance Heap h => Heap (BootstrapHeap h) where
    empty = E
    isEmpty E = True
    isEmpty _ = False
    insert x = merge (H x empty)
    merge E h = h
    merge h E = h
    merge h1@(H x p1) h2@(H y p2) = if x <= y
        then H x (insert h2 p1)
        else H y (insert h1 p2)
    findMin E = Nothing
    findMin (H x _) = Just x
    deleteMin E = Nothing
    deleteMin (H _ h') = rebuild $ pop h'
        where
            rebuild h' = case h' of
                Nothing -> Just E
                Just ((H x h''), h''') -> Just $ H x (merge h'' h''')
