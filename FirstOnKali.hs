module FirstOnKali (
                   ) where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = (*) x $ product' xs

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

mProduct :: Num a => [a] -> a
mProduct [] = 1
mProduct (x:xs) = (*) x . mProduct $ xs

type IntAction = Int -> Int -> Int

addInteger :: IntAction
addInteger = (+)

addOne :: Integer -> Integer
addOne = (+) 1

bindExp :: Integer -> String
bindExp x = let y = 5
            in
                let z = y + z
                in
                    "the integer was: "
                    <> show x <> "\n"
                    <> "and y was: "
                    <> show y <> "\n"
                    <> "and z was: "
                    <> show z <> "\n"

scopeExp :: Integer -> String
scopeExp x = let x = 10; y = 5 in
    "x: " <> show x <> "y: " <> show y

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = if odd n then (+ 1) n else n

mFlip :: (a -> b -> c) -> b -> a -> c
mFlip f y x = f x y

squares :: (Eq a, Num a) => a -> [a]
squares 0 = []
squares n = n ^ 2 : squares (n - 1) 

square :: Num a => a -> a
square = flip (^) 2

applyMath :: Num a => (a -> a) -> [a] -> [a]
applyMath _ [] = []
applyMath f (x:xs) = f x : applyMath f xs

