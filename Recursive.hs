module Recursive (
                 ) where
import GHC.Exts.Heap (GenClosure(value))

qsort :: (Eq a, Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (++) (qsort lesser) $ x : qsort greater
      where lesser = filter (<=x) xs
            greater = filter (>x) xs

insert :: (Eq a, Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:z:zs)
      | y < x && x < z = y:x:z:zs
      | otherwise = y : insert x (z:zs)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

mScanl :: (b -> a -> b) -> b -> [a] -> [b]
mScanl = scanlGo
    where
       scanlGo :: (b -> a -> b) -> b -> [a] -> [b]
       scanlGo f q ls = q : (case ls of
                               [] -> []
                               (x:xs) -> scanlGo f (f q x) xs)

facs :: [Int]
facs = mScanl (*) 1 [1..]

fac' :: Int -> Int
fac' n = facs !! n

y :: (t -> t) -> t
y f = f (y f)

facFixed :: Int -> Int
facFixed = y (\f n -> if n == 0 then 1 else n * f (n-1))

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes t n = 1 + incTimes (t - 1) n

applyTimes :: (Eq a, Num a) => 
      a -> (b -> b) -> b -> b
applyTimes 0 _ n = n
applyTimes t f n = f . applyTimes (t - 1) f $ n

applyTimes' :: (Eq a, Num a) =>
      a -> (b -> b) -> b -> b
applyTimes' 0 _ n = n
applyTimes' t f n = applyTimes' (t - 1) f (f n)

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1 ) + fib (n - 2)

fib' :: Integral a => a -> a
fib' n = go n 0 1
    where go 0 a _ = a
          go 1 _ b = b
          go m a b = go (m - 1) b (a + b)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy x y = go x y 0
      where go a b n
                  | a < b = n
                  | otherwise = go (subtract b a) b (n + 1)

indexed :: String -> [(Int, Char)]
indexed xs = go 0 xs
    where go _ [] = []
          go n (x:xs) = (n, x) : go (n+1) xs

greatestCommonDivisor :: (Int, Int) -> Int
greatestCommonDivisor (x,y)
      | x == y = x
      | x < y = greatestCommonDivisor (x, y - x)
      | otherwise = greatestCommonDivisor (x - y, y)

leastCommonMultiple :: (Int, Int) -> Int
leastCommonMultiple (x, y) = goLeast xs ys
      where xs = map (x *) [1..]
            ys = map (y *) [1..]
            goLeast (a:as) (b:bs)
                  | a == b = a
                  | a < b = goLeast as (b:bs)
                  | otherwise = goLeast (a:as) bs

myAnd :: [Bool] -> Bool
myAnd = go True
      where go v [] = v
            go v (x:xs)
                  | x = go v xs
                  | otherwise = False
            
myOr :: [Bool] -> Bool
myOr = go False
      where go v [] = v
            go v (x:xs)
                  | x = True
                  | otherwise = go v xs