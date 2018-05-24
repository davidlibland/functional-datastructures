{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RedBlackTree (RedBlackTree) where
import Prelude hiding (length)
import Set
import Data.Monoid

data Color = Red | Black deriving Show
data RedBlackTree a = E | T Color (RedBlackTree a) a (RedBlackTree a)
    deriving Show

balance :: Color -> (RedBlackTree a) -> a -> (RedBlackTree a) -> RedBlackTree a
balance Black (T Red (T Red ta x tb) y tc) z td
    = T Red (T Black ta x tb) y (T Black tc z td)
balance Black (T Red ta x (T Red tb y tc)) z td
    = T Red (T Black ta x tb) y (T Black tc z td)
balance Black ta x (T Red (T Red tb y tc) z td)
    = T Red (T Black ta x tb) y (T Black tc z td)
balance Black ta x (T Red tb y (T Red tc z td))
    = T Red (T Black ta x tb) y (T Black tc z td)
balance c t1 x t2 = T c t1 x t2

instance Ord a => Set RedBlackTree a where
    empty = E
    insert x s = T Black l y r
        where
            ins x E = T Red E x E
            ins x (T c l y r)
                | x < y = balance c (ins x l) y r
                | y < x = balance c l y (ins x r)
                | otherwise = (T c l y r)
            (T c l y r) = ins x s
    member x E = False
    member x (T _ l y r) = x == y || member x l || member x r

--     length E = 0
--     length (T _ l _ r) = 1 + (length l) + (length r)

instance Foldable (RedBlackTree) where
    foldMap f E = mempty
    foldMap f (T _ l x r) = (foldMap f l) <> (f x) <> (foldMap f r)