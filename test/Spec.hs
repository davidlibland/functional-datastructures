import Test.Hspec
import Test.QuickCheck
import Sorting
import Graph
import Data.List (sort)
import BottomUpMergeSort
import qualified BinaryRandomAccessList as BRAL
import qualified RandomAccessList as RAL
import qualified Sortable as SRT

bmsSort = SRT.sort . (SRT.fromList :: Ord a => [a] -> MergeSort a)

main :: IO ()
main = hspec $ do {
    describe "sorting" $ do {
        it "mergeSort is idempotent" $
            mergeSort(mergeSort [3,5,1,2]) `shouldBe` mergeSort [3,5,1,2];
        it "mergeSort sorts correctly" $ property $
            \a -> mergeSort a == sort (a::[Int]);

        it "quickSort is idempotent" $
            quickSort(quickSort [3,5,1,2]) `shouldBe` quickSort [3,5,1,2];
        it "quickSort sorts correctly" $ property $
            \a -> quickSort a == sort (a::[Int]);

        it "bottomUpMergeSort is idempotent" $
            bmsSort (bmsSort  [3,5,1,2]) `shouldBe` bmsSort [3,5,1,2];
        it "bottomUpMergeSort sorts correctly" $ property $
            \a -> bmsSort a == sort (a::[Int])
    };

    let g =(graphFromList [(1,2),(2,3),(1,3),(3,4)]) in
    describe "dijkstra" $ do {
        it "finds the shortest path" $
            dijkstra g (Node 1) (Node 4) `shouldBe` (Just 2);
        it "finds the shortest path using state" $
            dijkstraS g (Node 1) (Node 4) `shouldBe` (Just 2)
    };

    let {
        fromListS a = RAL.fromList a :: BRAL.BinaryList Int;
        toListS l = RAL.toList l;
        wrapS f = toListS . f . fromListS;
        wrapMS f = (fmap toListS) . f . fromListS
    } in
    describe "BinaryRandomAccessList" $ do {
        it "empty isEmpty" $
            RAL.isEmpty (RAL.empty ::
                BRAL.BinaryList Int)
                `shouldBe` True;
        it "singleton is not empty" $
            RAL.isEmpty (RAL.cons 1 (RAL.empty :: BRAL.BinaryList Int))
                `shouldBe` False;
        it "non-empty list is not empty" $
            RAL.isEmpty (fromListS [1,2,3])
                `shouldBe` False;
        it "head should yield the first entry" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                RAL.head (RAL.fromList a :: BRAL.BinaryList Int)
                == head a;
        it "tail should yield the remainder" $ property $
            \h t -> let {a :: [Int] ; a = h : t } in
                (RAL.toList $RAL.tail (RAL.fromList a :: BRAL.BinaryList Int))
                == tail a;
        it "lookup should yield the correct value" $ property $
            \h t i -> let {
                    a :: [Int];
                    a = h:t;
                    j :: Int;
                    j = i `mod` (length a);
                } in
                (RAL.lookup j (RAL.fromList a :: BRAL.BinaryList Int))
                == (Just (a !! j));
        it "invalid lookup should yield Nothing" $ property $
            \a i ->
                (RAL.lookup i (RAL.fromList a :: BRAL.BinaryList Int))
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
}