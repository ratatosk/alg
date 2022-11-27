module RAList 
    ( RAList
    ) where

import Tree

data Digit a = Zero | One (Tree a)
type RAList a = [Digit a]

fromRA :: RAList a -> [a]
fromRA = concatMap from
  where 
    from Zero = []
    from (One t) = fromT t

fetchRA :: Int -> RAList a -> a
fetchRA k (Zero : xs) = fetchRA k xs
fetchRA k (One t : xs) = if k < size t
    then fetchT k t 
    else fetchRA (k - size t) xs

fetchT :: Int -> Tree a -> a
fetchT 0 (Leaf x) = x
fetchT k (Node n t1 t2) = if k < m
    then fetchT k t1 
    else fetchT (k - m) t2
  where 
    m = n `div` 2