module SymList 
    ( SymList
    , nilSL
    , fromSL
    , lastSL
    , tailSL
    , initSL
    , consSL
    , snocSL    
    , singleSL
    , nullSL
    , dropWhileSL
    , singletonSL
    , initsSL
    ) where

data SymList a = SL [a] [a] deriving (Show)

nilSL :: SymList a
nilSL = SL [] []

fromSL :: SymList a -> [a]
fromSL (SL xs ys) = xs ++ reverse ys

snocSL :: a -> SymList a -> SymList a
snocSL x (SL xs ys) = if null xs then SL ys [x] else SL xs (x : ys)

consSL :: a -> SymList a -> SymList a
consSL x (SL xs ys) = if null ys then SL [x] xs else SL (x : xs) ys

headSL :: SymList a -> a
headSL (SL xs ys) =
    if null xs
    then if null ys
        then error "headSL of empty list"
        else head ys
    else head xs

lastSL :: SymList a -> a
lastSL (SL xs ys) = 
    if null ys 
    then if null xs
        then error "lastSL of empty list" 
        else head xs 
    else head ys

singletonSL :: a -> SymList a
singletonSL x = SL [x] []

single :: [a] -> Bool
single [_] = True
single _ = False

tailSL :: SymList a -> SymList a
tailSL (SL xs ys)
    | null xs = if null ys then error "tailSL of empty list" else nilSL
    | single xs = SL (reverse vs) us
    | otherwise = SL (tail xs) ys
  where 
    (us, vs) = splitAt (length ys `div` 2) ys

initSL :: SymList a -> SymList a
initSL (SL xs ys)
    | null ys = if null xs then error "initSL of empty list" else nilSL
    | single ys = SL us (reverse vs)
    | otherwise = SL xs (tail ys)
  where
    (us, vs) = splitAt (length xs `div` 2) xs

nullSL :: SymList a -> Bool
nullSL (SL x y) = null x && null y

singleSL :: SymList a -> Bool
singleSL (SL [] x) = single x
singleSL (SL x []) = single x
singleSL _ = False

lengthSL :: SymList a -> Int
lengthSL (SL x y) = length x + length y

dropWhileSL :: (a -> Bool) -> SymList a -> SymList a
dropWhileSL p xs
    | nullSL xs = nilSL
    | p (headSL xs) = dropWhileSL p (tailSL xs)
    | otherwise = xs

initsSL :: SymList a -> SymList (SymList a)
initsSL x
    | nullSL x = singletonSL nilSL
    | otherwise = snocSL x (initsSL (initSL x))