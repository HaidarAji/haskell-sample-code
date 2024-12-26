module HigherOrderFunc
    ( divideByTen
    , divideByTen'
    , applyTwice
    , applyFourTimes
    , foldr1'
    , elem'
    , reverse'
    , filter'
    , and'
    , reverseFold
    , sumRec
    , myFoldl
    ) where

import Data.Foldable
import Data.Maybe
import Data.Time.Format.ISO8601 (yearFormat)
import Data.Void (vacuous)

divideByTen :: (Floating a) => a -> a
divideByTen x = x / 10

divideByTen' :: (Floating a) => a -> a
divideByTen' = flip (/) 10

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

applyFourTimes :: (a -> a) -> a -> a
applyFourTimes = applyTwice . applyTwice

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x

chain :: Integer -> [Integer]
chain n
    | n <= 1 = [1]
    | even n = n : chain (div n 2)
    | otherwise = n : chain (n * 3 + 1)

ones :: [Int]
ones = 1 : ones

elem' :: Ord a => a -> [a] -> Bool
elem' x = foldr (\ y acc -> (x == y) || acc) False

foldr1' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1' f xs = fromMaybe (error "foldr1: empty structure")
                (foldr mf Nothing xs)
    where mf x m = Just (case m of
                           Nothing -> x
                           Just y -> f x y)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\ x acc -> if f x then x:acc else acc) []

filterRec :: (a -> Bool) -> [a] -> [a]
filterRec _pred [] = []
filterRec pred (x:xs)
    | pred x = x : filter pred xs
    | otherwise = filter pred xs

last' :: [a] -> a
last' [] = error "empty structure"
last' [x] = x
last' (x:xs) = last' xs

and' :: [Bool] -> Bool
and' = foldr (&&) True

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverseFold :: [a] -> [a]
reverseFold = foldr snoc []

sumRec :: Num a => [a] -> a
sumRec = sum' 0
    where sum' v [] = v
          sum' v (x:xs) = sum' (v+x) xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f v [] = v
myFoldl f v (x:xs) = myFoldl f (f v x) xs

data Employee = Coder
    | Manager
    | Veep
    | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e <> " is the boss of " <> show e'

employeeRank :: (Employee -> Employee -> Ordering)
            -> Employee
            -> Employee
            -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Employee is the Boss"
        LT -> reportBoss e' e

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

mFoldr :: (a -> b -> b) -> b -> [a] -> b
mFoldr f v [] = v
mFoldr f v (x:xs) = f x $ mFoldr f v xs

mFoldl :: (b -> a -> b) -> b -> [a] -> b
mFoldl f v [] = v
mFoldl f v (x:xs) = mFoldl f (f v x) xs