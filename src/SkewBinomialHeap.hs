{-# LANGUAGE TemplateHaskell #-}

module SkewBinomialHeap where
import Control.Lens hiding (children)
import Heap

data SBT a = SBT {
    _label :: a,
    _auxLabels :: [a],
    _rank :: Int,
    _children :: [SBT a]
    } deriving Show

makeLenses ''SBT

link :: Ord a => SBT a -> SBT a -> SBT a
link t1 t2 = if t1^.label > t2^.label
    then makeLink t1 t2
    else makeLink t2 t1
        where
            makeLink t t' = rank +~  1 $ children %~ (t:) $ t'

skewLink :: Ord a => a -> SBT a -> SBT a -> SBT a
skewLink x t1 t2 = if x < y
    then pushChild x y t
    else pushChild y x t
        where
            t = link t1 t2
            y = t^.label
            pushChild s l t = label.~ s $ auxLabels %~ (y:) $ t

newtype SkewBinomialHeap a = SBH [SBT a]

insertTree :: Ord a => SBT a -> [SBT a] -> [SBT a]
insertTree t [] = [t]
insertTree t (t':ts)
    | t^.rank < t'^.rank = (t:t':ts)
    | t^.rank > t'^.rank = error "improper tree insertion"
    | otherwise = insertTree (link t t') ts

popMinTree :: Ord a => [SBT a] ->
    Maybe (SBT a, [SBT a])
popMinTree [] = Nothing
popMinTree (t:ts) = case popMinTree ts of
    Nothing -> Just (t, [])
    Just (t', ts') -> if t^.label < t'^.label
        then Just (t, ts)
        else Just (t', t:ts')

mrg :: Ord a => [SBT a] -> [SBT a] -> [SBT a]
mrg [] h = h
mrg h [] = h
mrg  ts1@(t1:ts1')  ts2@(t2:ts2')
    | t1^.rank < t2^.rank = t1:mrg ts1' ts2
    | t2^.rank < t1^.rank = t2:mrg ts1 ts2'
    | otherwise = insertTree (link t1 t2) (mrg ts1' ts2')

normalize :: Ord a => [SBT a] -> [SBT a]
normalize [] = []
normalize (t:ts) = insertTree t ts

singleton x = SBT { _label=x, _rank=0, _children = [], _auxLabels = [] }

instance Heap SkewBinomialHeap where
    empty = SBH []
    isEmpty (SBH l) = null l

    insert x (SBH ts@(t1:t2:ts')) = if t1^.rank == t2^.rank
        then SBH $ insertTree (skewLink x t1 t2) ts'
        else SBH $ insertTree (singleton x) ts
    insert x (SBH ts) = SBH $ insertTree (singleton x) ts

    pop (SBH l) = case popMinTree l of
        Nothing -> Nothing
        Just (t, ts) -> Just (t^.label, foldl (flip insert) h3 (t^.auxLabels))
            where
                h1 = SBH $ reverse $ t^.children
                h2 = SBH ts
                h3 = (merge h1 h2)

    merge (SBH h1) (SBH h2) = SBH $ mrg (normalize h1) (normalize h2)