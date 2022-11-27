module Lib where

import Tree
import Data.List (inits, tails)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Exercise 1.4 Write down a definition of reverse that takes linear time.
-- One possibility is to use a foldl.

reverse2 :: [a] -> [a]
reverse2 l = help id l []
    where
        help f [] = f
        help f (x:xs) = help ((x:).f) xs

reverse3 :: [a] -> [a]
reverse3 l = foldl (\f x -> (x:).f) id l []

-- Answer:
reverse4 :: [a] -> [a]
reverse4 = foldl (flip (:)) []

-- Exercise 1.5 Express both map and filter as an instance of foldr.

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr ((:).f) []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x l -> if p x then x:l else l) []

-- Exercise 1.6 Express foldr f e · filter p as an instance of foldr.
target :: (a -> b -> b) -> b -> (a -> Bool) -> [a] -> b
target f e p = foldr f e . filter p

target2 :: (a -> b -> b) -> b -> (a -> Bool) -> [a] -> b
target2 f e p = foldr (\x y -> if p x then f x y else y) e

-- Exercise 1.7 The function takeWhile returns the longest initial segment of a list all
-- of whose elements satisfy a given test. Moreover, its running time is proportional to
-- the length of the result, not the length of the input. Express takeWhile as an instance
-- of foldr, thereby demonstrating once again that a foldr need not process the whole
-- of its argument before terminating. 

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p a = foldr op [] a
    where 
        op x xs = if p x then x:xs else []

-- Exercise 1.8 The Data.List library contains a function dropWhileEnd which drops
-- the longest suffix of a list all of whose elements satisfy a given Boolean test. For
-- example
--     dropWhileEnd even [1,4,3,6,2,4]=[1,4,3]
-- Define dropWhileEnd as an instance of foldr.

dropWhileEnd2 :: (a -> Bool) -> [a] -> [a]
dropWhileEnd2 p = foldr op [] 
    where
        op x xs = if p x && null xs then [] else x:xs

-- Exercise 1.11 Given a list of digits representing a natural number, construct a
-- function integer which converts the digits into that number. For example,
--     integer [1,4,8,4,9,3] = 148493
-- Next, given a list of digits representing a real number r in the range 0  r <
-- 1, construct a function fraction which converts the digits into the corresponding
-- fraction. For example,
--     fraction [1,4,8,4,9,3] = 0.148493

integer :: [Int] -> Int
integer = foldl op 0 where op a x = a*10 + x

fraction :: [Int] -> Double
fraction = foldr op 0 where op x a = (a + fromIntegral x) / 10

-- Exercise 1.12 Complete the right-hand sides of
--     map (foldl f e) · inits = ????
--     map (foldr f e) · tails = ????

testLI :: (b -> a -> b) -> b -> [a] -> [b]
testLI f e = map (foldl f e) . inits

myLI :: (b -> a -> b) -> b -> [a] -> [b]
myLI f e l = xs
    where
        xs = e : zipWith f xs l

myLI2 :: (b -> a -> b) -> b -> [a] -> [b]
myLI2 f e l = reverse $ foldl (\xs x -> f (head xs) x : xs) [e] l

answerLI :: (b -> a -> b) -> b -> [a] -> [b]
answerLI f e = scanl f e

testRT :: (a -> b -> b) -> b -> [a] -> [b]
testRT f e = map (foldr f e) . tails

myRT :: (a -> b -> b) -> b -> [a] -> [b]
myRT f e l = foldl (\xs x -> f x (head xs) : xs) [e] (reverse l)

answerRT :: (a -> b -> b) -> b -> [a] -> [b]
answerRT f e = scanr f e

-- Exercise 1.13 Define the function
--     apply ::Nat → (a → a) → a → a
-- that applies a function a specified number of times to a value.

apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = apply (n-1) f (f x)

-- Exercise 1.14 Can the function inserts associated with the inductive definition of
-- perms1 be expressed as an instance of foldr?

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y : ys) = (x : y : ys) : map (y:) (inserts x ys)

inserts2 :: a -> [a] -> [[a]]
inserts2 z xs = foldr f [[z]] xs
    where 
        f a b = (z:a:tail (head b)) : map (a:) b

-- Exercise 1.15 Give a definition of remove for which
--     perms3 [] = [[]]
--     perms3 xs = [x : ys | x ← xs,ys ← perms3 (remove x xs)]
-- computes the permutations of a list. Is the first clause necessary? What is the type
-- of perms3, and can one generate the permutations of a list of functions with this
-- definition?

perms3 :: Eq a => [a] -> [[a]]
perms3 [] = [[]]
perms3 xs = [x : ys | x <- xs, ys <- perms3 (remove x xs)]
    where
        remove a [] = []
        remove a (b:bs) = if a == b then bs else b : remove a bs 

-- Exercise 1.20 Find a definition of op so that
--     concat xss = foldl op id xss [ ]

testOp :: [[a]] -> [a]
testOp xss = foldl op id xss []
    where op f xs = (++xs) . f

-- Exercise 1.21 A list of numbers is said to be steep if each number is greater than the
-- sum of the elements following it. Give a simple definition of the Boolean function
-- steep for determining whether a sequence of numbers is steep. What is the running
-- time and how can you improve it by tupling?

isSteep1 :: [Int] -> Bool
isSteep1 [] = True
isSteep1 (x:xs) = x > sum xs && isSteep1 xs

isSteep2 :: [Int] -> Bool
isSteep2 [] = True
isSteep2 xs = and $ zipWith (>) xs (tail $ scanr (+) 0 xs)

-- Exercise 2.9 Use the fusion law of foldr to simplify head · concat1. Can the fusion
-- law of foldl be used to simplify head · concat2?
--
-- concat1,concat2 ::[[a]] → [a]
-- concat1 = foldr (++) [ ]
-- concat2 = foldl (++) [ ]

hconcatTest :: [[a]] -> a
hconcatTest = head . concat

hconcat1 :: [[a]] -> a
hconcat1 = foldr f undefined
    where f xs x = if null xs then x else head xs

-- some defs from the book

perms1 :: [a] -> [[a]]
perms1 = foldr (concatMap . inserts) [[]]
    where
        inserts x [] = [[x]]
        inserts x (y : ys) = (x : y : ys) : map (y:) (inserts x ys)

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = concatMap subperms (picks xs)
    where 
        subperms (x,ys) = map (x:) (perms2 ys)
        picks [] = []
        picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

--Exercise 2.12 Can the trick of using an accumulating function work with inits?

inits2 :: [a] -> [[a]]
inits2 = help id
    where
        help f [] = [f []]
        help f (y:ys) = f [] : help (f.(y:)) ys

-- Exercise 2.14 Using the function iterate, give a one-line definition of the function
-- tails1 that returns the nonempty suffixes of a list.

tails1 = takeWhile (not . null) . iterate tail

-- Exercise 3.7 Give an online definition of inits that does not use symmetric lists for
-- which length ·inits takes linear time.

inits1 :: [a] -> [[a]]
inits1 = map reverse . scanl (flip (:)) []

-- One way to reduce the running time is to introduce a function
-- fromTs:: [Tree a] → [a]
-- and define fromT t = fromTs [t]. Give an efficient definition of fromTs

fromTs:: [Tree a] -> [a]
fromTs [] = []
fromTs (Leaf x : ts) = x : fromTs ts
fromTs (Node _ l r : ts) = fromTs (l:r:ts)

