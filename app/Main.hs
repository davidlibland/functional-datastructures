module Main where
import System.Random
import Criterion.Main
import Sorting
import BottomUpMergeSort
import qualified Sortable as SRT
import qualified Data.List
import Heap
import LeftistHeap
import PairingHeap
import SplayHeap
import BinomialHeap

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

test :: Int -> IO [Int]
test n = sequence $ replicate n $ randomRIO (-100,100::Int)

-- Our benchmark harness.
main = do{
    testList <- test 100;
    testList1 <- test 100;
    testList2 <- test 100;
    testList3 <- test 100;
    defaultMain [
        bgroup "sorting" [ bench "quickSort"  $ nf quickSort testList
                   , bench "mergeSort"  $ nf quickSort testList
                   , bench "bottomUpMergeSort"  $ nf bmsSort testList
                   , bench "preludeSort"  $ nf Data.List.sort testList
                   ],

        let
            insertManyS xs = (insertMany xs)::LeftistHeap Int
            testHeap1 = (insertMany testList1) :: LeftistHeap Int
            deleteMany h = case deleteMin h of
                Just h' -> deleteMin h'
                Nothing -> Nothing
            testHeap2 = (insertMany testList2) :: LeftistHeap Int
            mergeS = merge testHeap2
            heapifyS xs = (heapify xs) :: LeftistHeap Int
            heapSort xs = id $! toList $ heapifyS xs
        in bgroup
            "LeftistHeap" [
                bench "insert"  $ whnf insertManyS testList3,
                bench "deleteMin"  $ whnf deleteMany testHeap1,
                bench "merge"  $ whnf mergeS testHeap1,
                bench "heapSort" $ whnf heapSort testList
            ],

        let
            insertManyS xs = (insertMany xs)::PairingHeap Int
            testHeap1 = (insertMany testList1) :: PairingHeap Int
            deleteMany h = case deleteMin h of
                Just h' -> deleteMin h'
                Nothing -> Nothing
            testHeap2 = (insertMany testList2) :: PairingHeap Int
            mergeS = merge testHeap2
            heapifyS xs = (heapify xs) :: PairingHeap Int
            heapSort xs = id $! toList $ heapifyS xs
        in bgroup
            "PairingHeap" [
                bench "insert"  $ whnf insertManyS testList3,
                bench "deleteMin"  $ whnf deleteMany testHeap1,
                bench "merge"  $ whnf mergeS testHeap1,
                bench "heapSort" $ whnf heapSort testList
            ],

        let
            insertManyS xs = (insertMany xs)::SplayHeap Int
            testHeap1 = (insertMany testList1) :: SplayHeap Int
            deleteMany h = case deleteMin h of
                Just h' -> deleteMin h'
                Nothing -> Nothing
            testHeap2 = (insertMany testList2) :: SplayHeap Int
            mergeS = merge testHeap2
            heapifyS xs = (heapify xs) :: SplayHeap Int
            heapSort xs = id $! toList $ heapifyS xs
        in bgroup
            "SplayHeap" [
                bench "insert"  $ whnf insertManyS testList3,
                bench "deleteMin"  $ whnf deleteMany testHeap1,
                bench "merge"  $ whnf mergeS testHeap1,
                bench "heapSort" $ whnf heapSort testList
            ],

        let
            insertManyS xs = (insertMany xs)::BinomialHeap Int
            testHeap1 = (insertMany testList1) :: BinomialHeap Int
            deleteMany h = case deleteMin h of
                Just h' -> deleteMin h'
                Nothing -> Nothing
            testHeap2 = (insertMany testList2) :: BinomialHeap Int
            mergeS = merge testHeap2
            heapifyS xs = (heapify xs) :: BinomialHeap Int
            heapSort xs = id $! toList $ heapifyS xs
        in bgroup
            "BinomialHeap" [
                bench "insert"  $ whnf insertManyS testList3,
                bench "deleteMin"  $ whnf deleteMany testHeap1,
                bench "merge"  $ whnf mergeS testHeap1,
                bench "heapSort" $ whnf heapSort testList
            ]
                ]
}

insertMany :: (Heap h, Ord a) => [a] -> h a
insertMany = foldr insert empty