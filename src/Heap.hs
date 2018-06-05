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
    heapify = mergeAll . map (foldr insert empty) . sequences
      where
        sequences (a:b:xs)
          | a > b = descending b [a]  xs
          | otherwise       = ascending  b (a:) xs
        sequences xs = [xs]

        descending a as (b:bs)
          | a > b = descending b (a:as) bs
        descending a as bs  = (a:as): sequences bs

        ascending a as (b:bs)
          | a <= b = ascending b (\ys -> as (a:ys)) bs
        ascending a as bs   = let !x = as [a]
                              in x : sequences bs

        mergeAll [x] = x
        mergeAll xs  = mergeAll (mergePairs xs)

        mergePairs (a:b:xs) = let !x = merge a b
                              in x : mergePairs xs
        mergePairs xs       = xs

    toList :: Ord a => h a -> [a]
    toList h = case pop h of
        Just (e, h') -> e:(toList h')
        Nothing -> []
