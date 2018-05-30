module BinaryRandomAccessList where
import RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a)
data Digit a = Zero | One (Tree a)
newtype BinaryList a = BL [Digit a]

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t [] = [One t]
consTree t (Zero:ds) = (One t):ds
consTree t ((One t'):ds) = Zero:(consTree (link t t') ds)

link :: Tree a -> Tree a -> Tree a
link t1@(Node n1 _ _) t2@(Node n2 _ _) = Node (n1 + n2) t1 t2
link t1@(Leaf _) t2@(Leaf _) = Node 2 t1 t2

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree [One t] = (t, [])
unconsTree ((One t):ds) = (t, Zero:ds)
unconsTree (Zero:ds) = let ((Node _ t1 t2), ds') = unconsTree ds in
    (t1, (One t2):ds')
unconsTree [] = error "empty random access list"

lookupTree :: Int -> Tree a -> a
lookupTree i (Leaf x) = x
lookupTree i (Node w t1 t2) = if i < w `div` 2
    then lookupTree i t1
    else lookupTree (i - (w `div` 2)) t2

updateTree :: Int -> a -> Tree a -> Tree a
updateTree i y (Leaf x) = Leaf y
updateTree i y (Node w t1 t2) = if i < w `div` 2
    then Node w (updateTree i y t1) t2
    else Node w t1 (updateTree (i - (w `div` 2)) y t2)

size :: Tree a -> Int
size (Node w _ _) = w
size _ = 1


instance RandomAccessList BinaryList where
    empty = BL []
    isEmpty (BL []) = True
    isEmpty _ = False
    cons x (BL ds) = BL $ consTree (Leaf x) ds
    head (BL ds) = let (Leaf x, _) = unconsTree ds in x
    tail (BL ds) = let (_, ds') = unconsTree ds in BL $ ds'
    lookup i (BL (Zero:ds)) = lookup i (BL ds)
    lookup i (BL ((One t):ds)) = if 0 <= i  && i < size t
        then Just $ lookupTree i t
        else lookup (i - (size t)) (BL ds)
    lookup i _ = Nothing
    update i y (BL ds) = case upd i y ds of
        Just ds' -> Just $ BL $ ds'
        Nothing -> Nothing
        where
            upd i y (Zero:ds) = (Just (:)) <*> (Just Zero) <*> upd i y ds
            upd i y (d@(One t):ds) = if 0 <= i  && i < size t
                then (Just $ (One $ updateTree i y t):ds)
                else (Just (:)) <*> (Just d) <*> upd (i - (size t)) y ds
            upd i _ _ = Nothing


