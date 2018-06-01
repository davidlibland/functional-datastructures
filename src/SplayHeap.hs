{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SplayHeap (SplayHeap(..)) where
import Heap
import Set

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving Show

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _ E = (E, E)
partition p (T a x b) =
    if x < p
        then case b of
            E -> (T a x E, E)
            T c y d -> if y < p
                then let (l, r) = partition p d
                    in (T (T a x c) y l, r)
                else let (l, r) = partition p c
                    in (T a x l, T r y d)
        else case a of
            E -> (E, T E x b)
            T c y d -> if y < p
                then let (l, r) = partition p d
                    in (T c y l, T r x b)
                else let (l, r) = partition p c
                    in (l, T r y (T d x b))

instance Heap SplayHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    insert p t = let (s, b) = partition p t in (T s p b)
    merge t E = t
    merge t (T a x b) = (T a' x b')
        where
            a' = merge a l
            b' = merge b r
            (l, r) = partition x t

    heapify = foldr Heap.insert Heap.empty

    pop E = Nothing
    pop (T a x b) = case pop a of
        Nothing -> Just (x, b)
        Just (y, c) -> Just (y, T c x b)

    toList E = []
    toList (T l x r) = (toList l) ++ [x] ++ (toList r)

instance Ord c => Set SplayHeap c where
    member x E = False
    member x (T a y b)
        | x < y = member x a
        | y < x = member x b
        | y == x = True
    insert = Heap.insert
    empty = Heap.empty