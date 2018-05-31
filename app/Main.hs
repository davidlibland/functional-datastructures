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
import Control.Lens (traverse)

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

test :: Int -> IO [Int]
test n = sequence $ replicate n $ randomRIO (-100,100::Int)

-- Our benchmark harness.
main = ((=<<) defaultMain) $ traverse id [
        benchSort "quickSort" 100 quickSort,
        benchSort "mergeSort" 100 mergeSort,
        benchSort "bottomUpMergeSort" 100 bmsSort,
        benchSort "preludeSort" 100 Data.List.sort,

        benchHeap "LeftistHeap" 100 (Heap.empty::LeftistHeap Int),
        benchHeap "PairingHeap" 100 (Heap.empty::PairingHeap Int),
        benchHeap "SplayHeap" 100 (Heap.empty::SplayHeap Int),
        benchHeap "BinomialHeap" 100 (Heap.empty::BinomialHeap Int)
    ]

benchSort :: String -> Int -> ([Int] -> [Int]) -> IO Benchmark
benchSort name n sorter = do{
    testList <- test 100;
    return $ bench (name ++ " " ++ (show n) ++ " integers") $ nf sorter
        testList
}

benchHeap :: Heap.Heap h => String -> Int -> h Int -> IO Benchmark
benchHeap name n emptyH = do{
    testList <- test n;
    testList1 <- test n;
    testList2 <- test n;
    testList3 <- test n;
    let
        heapifyS = foldr Heap.insert emptyH
        testHeap1 = heapifyS testList1
        testHeap2 = heapifyS testList2
        deleteMany h = case Heap.deleteMin h of
            Just h' -> Heap.deleteMin h'
            Nothing -> Nothing
        mergeS = Heap.merge testHeap2
        heapSort xs = id $! Heap.toList $ heapifyS xs
    in return $ bgroup
        (name ++ " of size " ++ (show n)) [
            bench "insert"  $ whnf heapifyS testList3,
            bench "deleteMin"  $ whnf deleteMany testHeap1,
            bench "merge"  $ whnf mergeS testHeap1,
            bench "heapSort" $ whnf heapSort testList
        ]
    }
