module Tree 
    ( Tree(..)
    , size
    , node
    , fromT
    ) where

data Tree a = Leaf a | Node Int (Tree a) (Tree a)

size :: Tree a -> Int
size (Leaf x) = 1
size (Node n _ _) = n

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

fromTs :: [Tree a] -> [a]
fromTs [] = []
fromTs (Leaf x : ts) = x : fromTs ts
fromTs (Node _ l r : ts) = fromTs (l:r:ts)

fromT :: Tree a -> [a]
fromT x = fromTs [x]
