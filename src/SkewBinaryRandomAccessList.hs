module SkewBinaryRandomAccessList where
import RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
newtype RList a = SL [(Int, Tree a)] deriving Show

lookupTree :: Int -> Int -> Tree a -> Maybe a
lookupTree 0 _ (Leaf x) = Just x
lookupTree i w (Node x t1 t2)
    | i == 0 = Just x
    | i <= h = lookupTree (i - 1) h t1
    | i > h = lookupTree (i - h - 1) h t2
    where h = w `div` 2
lookupTree _ _ _ = Nothing

updateTree :: Int -> a -> Int -> Tree a -> Maybe (Tree a)
updateTree 0 y _ (Leaf x) = Just (Leaf y)
updateTree i y w (Node x t1 t2)
    | i == 0 = Just (Node y t1 t2)
    | i <= h = fmap (\t1' -> (Node x t1' t2)) (updateTree (i - 1) y h t1)
    | i > h = fmap (\t2' -> (Node x t1 t2')) (updateTree (i - h - 1) y h t2)
    where h = w `div` 2
updateTree _ _ _ _ = Nothing

instance RandomAccessList RList where
    empty = SL []
    isEmpty (SL ts) = null ts
    cons x (SL ts@((w1,t1):(w2,t2):ts')) = if w1 == w2
        then SL $ (w1+w2+1, (Node x t1 t2)):ts'
        else SL $ (1, Leaf x):ts
    cons x (SL ts) = SL $ ((1, Leaf x):ts)
    head (SL ((w1,t1):ts)) = case t1 of
        Leaf x -> x
        Node x _ _ -> x
    head _ = error "empty random access list"
    tail (SL ((w,t):ts)) = case t of
        Leaf x -> SL ts
        Node x t1 t2 -> SL ((h,t1):(h,t2):ts)
            where h = w `div` 2
    tail _ = error "empty random access list"
    lookup i (SL ((w,t):ts)) = if i < w
        then lookupTree i w t
        else lookup (i - w) (SL (ts))
    lookup _ _ = Nothing
    update i y (SL ((w,t):ts)) = if i < w
        then fmap (\t' -> SL $ (w, t'):ts) (updateTree i y w t)
        else fmap (\(SL ts') -> SL $ (w,t):ts') (update (i - w) y (SL (ts)))
    update _ _ _ = Nothing