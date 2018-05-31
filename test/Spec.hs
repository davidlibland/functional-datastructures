import Test.Hspec
import Test.QuickCheck
import Sorting
import Graph
import Data.List (sort)
import BottomUpMergeSort
import qualified BinaryRandomAccessList as BRAL
import qualified SkewBinaryRandomAccessList as SBRAL
import qualified RandomAccessList as RAL
import qualified Sortable as SRT

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