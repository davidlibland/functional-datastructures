import Test.Hspec
import Test.QuickCheck
import Sorting
import Graph
import Data.List (sort)

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
            \a -> quickSort a == sort (a::[Int])
    };

    let g =(graphFromList [(1,2),(2,3),(1,3),(3,4)]) in
    describe "dijkstra" $ do {
        it "finds the shortest path" $
            dijkstra g (Node 1) (Node 4) `shouldBe` (Just 2);
        it "finds the shortest path using state" $
            dijkstraS g (Node 1) (Node 4) `shouldBe` (Just 2)
    }
}