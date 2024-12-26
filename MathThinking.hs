module MathThinking where
import Distribution.PackageDescription (libVersionNumberShow)
import GHC.Exts.Heap.Closures (GenClosure(value))

type Prime = Integer

isFactor :: Integer -> Integer -> Bool
isFactor x y
    | y `mod` x == 0 = True
    | otherwise = False

factors :: Integer -> [Integer]
factors 1 = [1]
factors 2 = [1,2]
factors x = filter (`isFactor` x) numbers
    where numbers = [1..(x `div` 2)] ++ [x]

isPrime :: Integer -> Bool
isPrime x
    | factors x == [1,x] = True
    | otherwise = False

primes :: [Integer]
primes = [ x | x <- [2..], isPrime x ]

proofUnlimitedPrime :: Int -> Integer
proofUnlimitedPrime x = prodPrimes x + 1
    where prodPrimes = product . flip take primes

proof :: [Prime] -> Integer
proof = (+) 1 . product

myAnd :: [Bool] -> Bool
myAnd = go True
    where go v [] = v
          go v (y:ys)
              | y = go v ys
              | otherwise = False

myOr :: [Bool] -> Bool
myOr = go False
    where go v [] = v
          go v (y:ys)
              | y = True
              | otherwise = go v ys