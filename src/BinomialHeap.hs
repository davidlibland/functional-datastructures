{-# LANGUAGE TemplateHaskell #-}

module BinomialHeap where
import Control.Lens hiding (children)
import Heap

data BinomialTree a = BinomialTree {_label:: a, _rank::Int,
    _children::[BinomialTree a]} deriving Show

makeLenses ''BinomialTree

link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1 t2 = if t1^.label > t2^.label
    then makeLink t1 t2
    else makeLink t2 t1
        where
            makeLink t t' = rank +~  1 $ children %~ (t:) $ t'

newtype BinomialHeap a = BH [BinomialTree a]

insertTree :: Ord a => BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insertTree t [] = [t]
insertTree t (t':ts)
    | t^.rank < t'^.rank = (t:t':ts)
    | t^.rank > t'^.rank = (t':(insertTree t ts))
    | otherwise = ((link t t'):ts)

popMinTree :: Ord a => [BinomialTree a] ->
    Maybe (BinomialTree a, [BinomialTree a])
popMinTree [] = Nothing
popMinTree (t:ts) = case popMinTree ts of
    Nothing -> Just (t, [])
    Just (t', ts') -> if t^.label < t'^.label
        then Just (t, ts)
        else Just (t', t:ts')

instance Heap BinomialHeap where
    empty = BH []
    isEmpty (BH l) = null l

    merge (BH []) h = h
    merge (BH (t:ts)) (BH l) =
        merge (BH ts) $ BH (insertTree t l)

    insert x (BH l) = BH (insertTree t l)
        where
            t = BinomialTree { _label=x, _rank=0, _children = [] }

    pop (BH l) = case popMinTree l of
        Nothing -> Nothing
        Just (t, ts) -> Just (t^.label, merge h1 h2)
            where
                h1 = BH $ reverse $ t^.children
                h2 = BH ts