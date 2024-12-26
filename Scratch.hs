module Scratch (
    ) where
import Language.Haskell.TH (unsafe)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

unsafeTake :: Int -> [a] -> [a]
unsafeTake !_ [] = []
unsafeTake 1 (x:_) = [x]
unsafeTake m (x:xs) = x : unsafeTake (m - 1) xs

mTake :: Int -> [a] -> [a]
mTake n (x:xs)
    | 0 < n = x : unsafeTake (n-1) xs
    | otherwise = []

unsafeFibonacci :: Int -> Int
unsafeFibonacci 0 = 1
unsafeFibonacci n = n * unsafeFibonacci (n-1)

fib :: Int -> Int
fib n 
    | n < 0 = n
    | otherwise = unsafeFibonacci n

qSort :: (Eq a, Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) = let greater = filter (>=x) xs;
                   lesser = filter (<x) xs in
                    qSort lesser ++ x : qSort greater

twice :: (a -> a) -> a -> a
twice f = f . f

qSort' :: (Eq a, Ord a) => [a] -> [a]
qSort' [] = []
qSort' (x:xs) = let greater = [ y | y <- xs, y >= x];
                    lesser = [ z | z <- xs, z < x ] in
                        qSort' lesser ++ x : qSort' greater
