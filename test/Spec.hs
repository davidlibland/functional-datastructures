import Test.Hspec
import Test.QuickCheck
import Data.Maybe (fromJust)
import Sorting
import Graph
import Data.List (sort)
import BottomUpMergeSort
import qualified Sortable as SRT
import qualified BinaryRandomAccessList as BRAL
import qualified SkewBinaryRandomAccessList as SBRAL
import qualified RandomAccessList as RAL
import qualified Heap
import qualified LeftistHeap as LH
import qualified SplayHeap as SH
import qualified BinomialHeap as BH
import qualified SkewBinomialHeap as SBH

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

main :: IO ()
main = hspec $ do {
    sortingTests "mergeSort" mergeSort;
    sortingTests "quickSort" quickSort;
    sortingTests "bottomUpMergeSort" bmsSort;

    let g =(graphFromList [(1,2),(2,3),(1,3),(3,4)]) in
    describe "dijkstra" $ do {
        it "finds the shortest path" $
            dijkstra g (Node 1) (Node 4) `shouldBe` (Just 2);
        it "finds the shortest path using state" $
            dijkstraS g (Node 1) (Node 4) `shouldBe` (Just 2)
    };

    randomAccessListTests "BinaryRandomAccessList"
        (RAL.empty :: BRAL.BinaryList Int);

    randomAccessListTests "SkewBinaryRandomAccessList"
        (RAL.empty :: SBRAL.RList Int);

    heapTests "LeftistHeap" (Heap.empty :: LH.LeftistHeap Int);
    heapTests "SplayHeap" (Heap.empty :: SH.SplayHeap Int);
    heapTests "BinomialHeap" (Heap.empty :: BH.BinomialHeap Int);
    heapTests "SkewBinomialHeap" (Heap.empty :: SBH.SkewBinomialHeap Int);
}

sortingTests :: String -> ([Int] -> [Int]) -> Spec
sortingTests name sorter =
    describe ("sorting with " ++ name) $ do {
       it (name ++ " is idempotent") $
           sorter(sorter [3,5,1,2]) `shouldBe` sorter [3,5,1,2];
       it (name ++ " sorts correctly") $ property $
           \a -> sorter a == sort (a::[Int]);
   };

randomAccessListTests :: RAL.RandomAccessList r => String -> r Int -> Spec
randomAccessListTests name emptyL = let {
        fromListS = foldr RAL.cons emptyL;
        toListS l = RAL.toList l;
        wrapS f = toListS . f . fromListS;
        wrapMS f = (fmap toListS) . f . fromListS
    } in
    describe name $ do {
        it "empty isEmpty" $
            RAL.isEmpty emptyL
                `shouldBe` True;
        it "singleton is not empty" $
            RAL.isEmpty (RAL.cons 1 emptyL)
                `shouldBe` False;
        it "non-empty list is not empty" $
            RAL.isEmpty (fromListS [1,2,3])
                `shouldBe` False;
        it "head should yield the first entry" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                RAL.head (fromListS a)
                == head a;
        it "tail should yield the remainder" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                (RAL.toList $RAL.tail (fromListS a))
                == tail a;
        it "lookup should yield the correct value" $ property $
            \h t i -> let {
                    a :: [Int];
                    a = h:t;
                    j :: Int;
                    j = i `mod` (length a);
                } in
                (RAL.lookup j (fromListS a))
                == (Just (a !! j));
        it "invalid lookup should yield Nothing" $ property $
            \a i ->
                (RAL.lookup i (fromListS a))
                == if (0 <= i) && (i < (length a))
                    then (Just (a !! i))
                    else Nothing;
        it "update should yield the correct value" $ property $
            \h t i k -> let {
                    a :: [Int];
                    a = h:t;
                    j :: Int;
                    j = i `mod` (length a);
                } in
                wrapMS (RAL.update j (k :: Int)) a
                == (Just (take j a ++ [k] ++ drop (j+1) a));
    }

heapTests :: Heap.Heap h => String -> h Int -> Spec
heapTests name emptyH = let {
        fromListS = foldr Heap.insert emptyH;
        toListS h = Heap.toList h;
        wrapS f = toListS . f . fromListS;
        wrapMS f = (fmap toListS) . f . fromListS
    } in
    describe name $ do {
        it "empty isEmpty" $
            Heap.isEmpty emptyH
                `shouldBe` True;
        it "singleton is not empty" $
            Heap.isEmpty (Heap.insert 1 emptyH)
                `shouldBe` False;
        it "non-empty heap is not empty" $
            Heap.isEmpty (fromListS [1,2,3])
                `shouldBe` False;
        it "findMin should yield the first entry" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                fromJust (Heap.findMin (fromListS a))
                == head (sort a);
        it "deleteMin should yield the remainder" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                wrapS (fromJust . Heap.deleteMin) a
                == tail (sort a);
        it "toList should sort the entries" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                wrapS id a
                == sort a;
        it "pop should yield the first entry" $ property $
            \h t -> let {a :: [Int] ; a = h : t; } in
                wrapMS (fromJust . Heap.pop) a
                == (head (sort a), wrapS (fromJust . Heap.deleteMin) a);
        it "merge should yield a well formed heap" $ property $
            \h t h' t' -> let {
                a :: [Int];
                a = h : t;
                a' :: [Int];
                a' = h' : t'; } in
                toListS (Heap.merge (fromListS a) (fromListS a'))
                == sort (a ++ a');
    }