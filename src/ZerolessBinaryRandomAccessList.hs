module ZerolessBinaryRandomAccessList where
import Prelude hiding (lookup)
import RandomAccessList

data RList a = Nil
    | One a (RList (a, a))
    | Two a a (RList (a, a))
    | Three a a a (RList (a, a))
    deriving Show

uncons :: RList a -> (a, RList a)
uncons (One x Nil) = (x, Nil)
uncons (Two x y ts) = (x, One y ts)
uncons (Three x y z ts) = (x, Two y z ts)
uncons (One x ts) = let ((y, z), ts') = uncons ts in
    (x, Two y z ts')
uncons Nil = error "Empty list"

fupdate :: Int -> (a -> a) -> RList a -> Maybe (RList a)
fupdate 0 f (One x ts) = Just $ One (f x) ts
fupdate 0 f (Two x y ts) = Just $ Two (f x) y ts
fupdate i f (Two x y ts) = fmap (cons x) $ fupdate (i-1) f $ One y ts
fupdate 0 f (Three x y z ts) = Just $ Three (f x) y z ts
fupdate i f (Three x y z ts) = fmap (cons x) $ fupdate (i - 1) f $ Two y z ts
fupdate i f (One x ts) = case ((i - 1) `mod` 2) of
    0 -> fmap (One x) $ fupdate j (\(x,y) -> (f x, y)) ts
    1 -> fmap (One x) $ fupdate j (\(x,y) -> (x, f y)) ts
    where j = ((i - 1) `div` 2)
fupdate _ _ _ = Nothing

instance RandomAccessList RList where
    empty = Nil
    isEmpty Nil = True
    isEmpty _ = False
    cons x Nil = One x Nil
    cons x (One y ts) = Two x y ts
    cons x (Two y z ts) = Three x y z ts
    cons x (Three w y z ts) = Two x w (cons (y, z) ts)
    head l = let (x, l') = uncons l in x
    tail l = let (x, l') = uncons l in l'
    lookup 0 (One x _) = Just x
    lookup 0 (Two x _ ts) = Just x
    lookup i (Two _ x ts) = lookup (i - 1) $ One x ts
    lookup 0 (Three x _ _ _) = Just x
    lookup i (Three _ x y ts) = lookup (i - 1) $ Two x y ts
    lookup i (One _ ts) = case ((i - 1) `mod` 2) of
        0 -> fmap fst p
        1 -> fmap snd p
        where p = lookup ((i - 1) `div` 2) ts
    lookup _ _ = Nothing
    update i y ts = fupdate i (\x -> y) ts