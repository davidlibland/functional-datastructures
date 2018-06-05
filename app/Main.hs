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
import SkewBinomialHeap
import qualified BootstrapHeap as BSH
import qualified RandomAccessList as RAL
import qualified BinaryRandomAccessList as BRAL
import qualified SkewBinaryRandomAccessList as SBRAL
import qualified ZerolessBinaryRandomAccessList as ZBRAL
import Control.Lens (traverse)
import Data.Maybe (fromJust)

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

test :: Int -> IO [Int]
test n = id $! sequence $ replicate n $ randomRIO (-100,100::Int)

n = 1000
m = 100
k = 100

-- Our benchmark harness.
main = ((=<<) defaultMain) $ traverse id [
        benchSort "quickSort" n quickSort,
        benchSort "mergeSort" n mergeSort,
        benchSort "bottomUpMergeSort" n bmsSort,
        benchSort "preludeSort" n Data.List.sort,

        benchHeap "LeftistHeap" n m k (Heap.empty::LeftistHeap Int),
        benchHeap "PairingHeap" n m k (Heap.empty::PairingHeap Int),
        benchHeap "SplayHeap" n m k (Heap.empty::SplayHeap Int),
        benchHeap "BinomialHeap" n m k (Heap.empty::BinomialHeap Int),
        benchHeap "SkewBinomialHeap" n m k (Heap.empty::SkewBinomialHeap Int),
        benchHeap "BootstrapSkewBinomialHeap" n m k
            (Heap.empty::BSH.BootstrapHeap SkewBinomialHeap Int),

        benchRAL "BinaryRandomAccessList" n (RAL.empty :: BRAL.BinaryList Int),
        benchRAL "SkewBinaryRandomAccessList" n (RAL.empty :: SBRAL.RList Int),
        benchRAL "ZerolessBinaryRandomAccessList" n (RAL.empty :: ZBRAL.RList
        Int)
    ]

benchSort :: String -> Int -> ([Int] -> [Int]) -> IO Benchmark
benchSort name n sorter = do{
    testList <- test n;
    return $ bench (name ++ " " ++ (show n) ++ " integers") $ nf sorter
        testList
}

benchHeap :: Heap.Heap h => String -> Int -> Int -> Int ->  h Int
    -> IO Benchmark
benchHeap name n m k emptyH = do{
    testList1 <- test n;
    testList2 <- test n;
    testList3i <- test n;
    testList3m <- test n;
    testList4 <- test n;
    testList5 <- test n;
    testList6i <- test n;
    testList6m <- test n;
    testLists <- traverse id (map test $ take k $ repeat n);
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
        testHeaps = map insertManyS testLists
        mergeAll hs = id $! foldr Heap.merge emptyH hs
        mergeDesc = "merge " ++ (show k) ++ " heaps of size " ++ (show m)
        sizeDesc = " of size " ++ (show n)
    in return $ bgroup
        (name) [
            bench ("insert from list" ++ sizeDesc) $ whnf insertManyS testList1,
            bench ("heapify (binary merge)" ++ sizeDesc) $ whnf heapifyS testList2,
            bench ("toList from insertion" ++ sizeDesc) $ nf Heap.toList
                testHeap3i,
            bench ("toList from binary merge" ++ sizeDesc) $ nf Heap.toList
                testHeap3m,
            bench ("deleteMin" ++ sizeDesc) $ whnf deleteMany testHeap4,
            bench mergeDesc $ whnf mergeAll testHeaps,
            bench ("insertion heapSort" ++ sizeDesc) $ nf iHeapSort testList6i,
            bench ("binary merge heapSort" ++ sizeDesc) $ nf mHeapSort
                testList6m
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