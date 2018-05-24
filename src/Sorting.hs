module Sorting where
import Control.Lens

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge s1 s2
    where
        (s1, s2) = both %~ mergeSort $ splitAt half l
        half = (((length l) + 1) `div` 2)
        merge s [] = s
        merge [] s = s
        merge sl@(hl:tl) sr@(hr:tr) = if hl < hr
            then hl:(merge tl sr)
            else hr:(merge sl tr)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (t:ts) = (quickSort l) ++ [t] ++ (quickSort r)
    where
        (l, r) = partition (<t) ts
        partition _ [] = ([], [])
        partition p (x:xs) = if p x
            then (x:l, r)
            else (l, x:r)
                where (l, r) = partition p xs
