module Main where
import System.Random
import Criterion.Main
import Sorting
import BottomUpMergeSort
import qualified Sortable as SRT
import qualified Data.List
import qualified Heap
import LeftistHeap
import PairingHeap
import SplayHeap
import BinomialHeap
import qualified RandomAccessList as RAL
import qualified BinaryRandomAccessList as BRAL
import qualified SkewBinaryRandomAccessList as SBRAL
import Control.Lens (traverse)
import Data.Maybe (fromJust)

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

test :: Int -> IO [Int]
test n = id $! sequence $ replicate n $ randomRIO (-100,100::Int)

n = 1000

-- Our benchmark harness.
main = ((=<<) defaultMain) $ traverse id [
        benchSort "quickSort" n quickSort,
        benchSort "mergeSort" n mergeSort,
        benchSort "bottomUpMergeSort" n bmsSort,
        benchSort "preludeSort" n Data.List.sort,

        benchHeap "LeftistHeap" n (Heap.empty::LeftistHeap Int),
        benchHeap "PairingHeap" n (Heap.empty::PairingHeap Int),
        benchHeap "SplayHeap" n (Heap.empty::SplayHeap Int),
        benchHeap "BinomialHeap" n (Heap.empty::BinomialHeap Int),

        benchRAL "BinaryRandomAccessList" n (RAL.empty :: BRAL.BinaryList Int),
        benchRAL "SkewBinaryRandomAccessList" n (RAL.empty :: SBRAL.RList Int)
    ]

benchSort :: String -> Int -> ([Int] -> [Int]) -> IO Benchmark
benchSort name n sorter = do{
    testList <- test n;
    return $ bench (name ++ " " ++ (show n) ++ " integers") $ nf sorter
        testList
}

benchHeap :: Heap.Heap h => String -> Int -> h Int -> IO Benchmark
benchHeap name n emptyH = do{
    testList1 <- test n;
    testList2 <- test n;
    testList3i <- test n;
    testList3m <- test n;
    testList4 <- test n;
    testList5 <- test n;
    testList6i <- test n;
    testList6m <- test n;
    let
        insertManyS xs = id $! foldr Heap.insert emptyH xs
        heapifyS xs = id $! Heap.merge (Heap.heapify xs) emptyH
        testHeap3i = insertManyS testList3i
        testHeap3m = heapifyS testList3m
        testHeap4 = heapifyS testList4
        testHeap5 = heapifyS testList5
        deleteMany h = case Heap.deleteMin h of
            Just h' -> Heap.deleteMin $! h'
            Nothing -> Nothing
        mergeS x = id $! Heap.merge testHeap4 x
        iHeapSort xs = id $! Heap.toList $ insertManyS xs
        mHeapSort xs = id $! Heap.toList $ heapifyS xs
    in return $ bgroup
        (name ++ " of size " ++ (show n)) [
            bench "insert"  $ whnf insertManyS testList1,
            bench "heapify (binary merge)"  $ whnf heapifyS testList2,
            bench "toList from insertion"  $ nf Heap.toList testHeap3i,
            bench "toList from binary merge"  $ nf Heap.toList testHeap3m,
            bench "deleteMin"  $ whnf deleteMany testHeap4,
            bench "merge"  $ whnf mergeS testHeap5,
            bench "insertion heapSort" $ nf iHeapSort testList6i,
            bench "binary merge heapSort" $ nf mHeapSort testList6m
        ]
    }

benchRAL :: RAL.RandomAccessList r => String -> Int -> r Int -> IO Benchmark
benchRAL name n emptyL = do{
    testList1 <- test n;
    testList2 <- test n;
    testList3 <- test n;
    testList4 <- test n;
    let
        buildListS xs = id $! foldr RAL.cons emptyL xs
        testRAL2 = buildListS testList2
        testRAL3 = buildListS testList3
        testRAL4 = buildListS testList4
        lookupMany i l = if i < n
            then (:) (RAL.lookup i l) $! (lookupMany (i+1) l)
            else []
        updateMany i l = if i < n
            then fromJust $! RAL.update i i (updateMany (i+1) l)
            else l
    in return $ bgroup
        (name ++ " of size " ++ (show n)) [
            bench "cons"  $ whnf buildListS testList1,
            bench "head & tail"  $ whnf RAL.toList testRAL2,
            bench "lookup"  $ whnf (lookupMany 0) testRAL3,
            bench "update"  $ whnf (updateMany 0) testRAL4
        ]
    }