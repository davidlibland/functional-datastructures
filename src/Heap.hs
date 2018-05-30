module Heap (Heap(..)) where
import Control.Lens

class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool

    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a

    pop :: Ord a => h a -> Maybe (a, h a)
    pop x = (Just (,)) <*> (findMin x) <*> (deleteMin x)

    findMin :: Ord a => h a -> Maybe a
    findMin = fmap fst . pop

    deleteMin :: Ord a => h a -> Maybe (h a)
    deleteMin = fmap snd . pop

    heapify :: Ord a => [a] -> h a
    heapify [] = empty
    heapify [x] = insert x empty
    heapify l = merge h1 h2 where
        (h1, h2) = over both heapify $ halve l

    toList :: Ord a => h a -> [a]
    toList h = case pop h of
        Just (e, h') -> e:(toList h')
        Nothing -> []

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x],[])
halve (x:y:zs) = (x:xs, y:ys) where (xs, ys) = halve zs