module Sorting where
import Control.Lens

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge s1 s2
    where
        (s1, s2) = both %~ mergeSort $ halve l
        halve [] = ([], [])
        halve [x] = ([x],[])
        halve (x:y:zs) = (x:xs, y:ys) where (xs, ys) = halve zs
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
