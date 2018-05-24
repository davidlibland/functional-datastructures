{-# LANGUAGE MultiParamTypeClasses #-}

module Set (Set(..)) where

class Set s a where
    empty :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool
--     length :: s a -> Int

    fromList :: [a] -> s a
    fromList [] = empty
    fromList (t:ts) = insert t (fromList ts)