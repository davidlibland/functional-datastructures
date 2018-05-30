module RandomAccessList (RandomAccessList(..))  where
import Prelude hiding (head, tail, lookup)

class RandomAccessList h where
    empty :: h a
    isEmpty :: h a -> Bool
    cons :: a -> h a -> h a
    head :: h a -> a
    tail :: h a -> h a
    lookup :: Int -> h a -> Maybe a
    update :: Int -> a -> h a -> Maybe (h a)
    fromList :: [a] -> h a
    fromList = foldr cons empty
    toList :: h a -> [a]
    toList l
        | isEmpty l = []
        | otherwise = (head l):(toList (tail l))

