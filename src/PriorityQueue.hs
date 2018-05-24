{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module PriorityQueue (PriorityQueue (..), SimpleHeapQueue)
where
import Control.Lens
import qualified Heap
import Data.Maybe (fromJust)

class Ord a => PriorityQueue q k a where
    empty :: q k a
    isEmpty :: q k a -> Bool

    insert :: k -> a -> q k a -> q k a

    pop :: q k a -> Maybe (k, a, q k a)
    pop x = do {
        (minKey, minRank) <- findMin x;
        newQ <- deleteMin x;
        return (minKey, minRank, newQ)
    }

    findMin :: q k a -> Maybe (k, a)
    findMin = fmap (\(key,val,_) -> (key, val)) . pop

    deleteMin :: q k a -> Maybe (q k a)
    deleteMin = fmap (\(_,_,newQ) -> newQ) . pop

    decreaseKey :: k -> a -> a -> q k a -> q k a

data SimpleHeapQueue h k a = SimpleHeapQueue {
    _positive:: h (a, k),
    _negative:: h (a, k)
}
makeLenses ''SimpleHeapQueue

normalize :: (Ord a, Heap.Heap h, Ord k) =>
    SimpleHeapQueue h k a -> SimpleHeapQueue h k a
normalize q
    | k2 == Nothing = q
    | k1 /= k2 = q
    | otherwise = normalize
         $ fromJust
         $ (>>= mapMOf positive Heap.deleteMin)
         $ mapMOf negative Heap.deleteMin q
         where
             k1 = Heap.findMin (q^.positive)
             k2 = Heap.findMin (q^.negative)

instance (Ord a, Heap.Heap h, Ord k) =>
    PriorityQueue (SimpleHeapQueue h) k a where
    empty = SimpleHeapQueue {
                _positive = Heap.empty,
                _negative = Heap.empty
            }

    isEmpty q
        | Heap.isEmpty ((normalize q)^.positive) = True
        | otherwise = False

    insert k a q = over positive (Heap.insert (a, k)) q

    pop q = let q' = normalize q in do {
        ((r, x), p') <- Heap.pop (q'^.positive);
        return (x, r, (positive.~p') q')
    }

    decreaseKey x a1 a2 =
        (negative%~(Heap.insert (a1, x))) .
        (positive%~(Heap.insert (a2, x)))

instance (Ord a, Ord k, Heap.Heap h, Show a, Show k) =>
    Show (SimpleHeapQueue h k a) where
    show q = concat $ map (\(a, k) -> "(" ++ show k ++ " " ++ show a ++ ") ")
        (Heap.toList ((normalize q)^.positive))