module BottomUpMergeSort (MergeSort) where
import Sortable

data MergeSort a = MS Int [[a]]

mrg [] ts = ts
mrg ts [] = ts
mrg xs@(x:xs') ys@(y:ys') = if x < y
    then x:(mrg xs' ys)
    else y:(mrg xs ys')

instance Sortable MergeSort where
    empty = MS 0 []
    add x (MS size segs) = MS (size + 1) (addSeg [x] segs size)
        where
            addSeg seg segs size
                | size `mod` 2 == 0 = (seg:segs)
                | otherwise = addSeg (mrg seg (head segs)) (tail segs) (size `div` 2)
    sort (MS _ xs) = foldl mrg [] xs