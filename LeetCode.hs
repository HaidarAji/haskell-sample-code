module LeetCode (
                ) where

import Data.Monoid
import Data.List
import Data.Maybe

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ (x : qsort greater)
    where lesser = filter (<=x) xs
          greater = filter (>x) xs

sortByParity :: [Int] -> [Int]
sortByParity xs = sortedEven ++ sortedOdd
    where sortedEven = qsort [ y | y <- xs, even y ]
          sortedOdd = qsort [ z | z <- xs, odd z ]

sortByParity' :: [Int] -> [Int]
sortByParity' xs = evens ++ odds
    where evens = [ y | y <- xs, even y ]
          odds  = [ z | z <- xs, odd z ]

sumTwo :: [Int] -> Int -> [Int]
sumTwo xs n = head [ [i, j] | (x,i) <- indexed, (y,j) <- indexed, y + x == n ]
    where indexed = zip xs [0..]

isPalindrome :: (Eq a, Ord a) => [a] -> Bool
isPalindrome xs = getAll . mconcat $ [ All y | (a,b) <- zip xs (reverse xs), let y = a == b ]

isPalindrome' :: (Eq a, Ord a) => [a] -> Bool
isPalindrome' xs = getAll . mconcat . map (All . (\(x,y) -> x == y)) $ zip xs (reverse xs)

isPalindrome'' :: (Eq a, Ord a) => [a] -> Bool
isPalindrome'' [] = False
isPalindrome'' [x] = True
isPalindrome'' [x,y] = x == y
isPalindrome'' xs = head xs == last xs && isPalindrome'' (tail . init $ xs)

romanMap :: [(Char,Int)]
romanMap = zip ['I','V','X','L','C','D','M'] [1,5,10,50,100,500,1000]

findInteger :: Char -> Int
findInteger = fromMaybe 0 . flip lookup romanMap

romanToInteger :: [Char] -> Int
romanToInteger romans = getSum . mconcat $ go romans
    where go [] = []
          go [x] = [Sum $ findInteger x]
          go (x:y:ys)
              | findInteger x < findInteger y = Sum (findInteger y - findInteger x) : go ys
              | otherwise = Sum (findInteger x) : go (y:ys)

longestCPrefix :: (Ord a, Eq a) => [[a]] -> [a]
longestCPrefix [] = []
longestCPrefix [x] = x
longestCPrefix xs
    | compareVal vals = head vals : longestCPrefix rest
    | otherwise = []
    where compareVal [] = False
          compareVal (y:ys) = getAll $ mconcat $ (All .) . (==) <$> [y] <*> ys
          vals = fromMaybe [] (mapM headMay xs)
          rest = fromMaybe [] (mapM tailMay xs)

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a, Eq a) => [a] -> Tree a
treeInsert [] = EmptyTree
treeInsert [x] = singletonTree x
treeInsert xs = Node pivot (treeInsert less) (treeInsert greater)
    where pivot
            | even (length xs) = (!!) xs $ length xs `div` 2 - 1
            | otherwise = (!!) xs $ length xs `div` 2
          less = filter (<pivot) xs
          greater = filter (>pivot) xs

treeInsertIndex :: (Ord a,Eq a) => [a] -> Tree (a,Int)
treeInsertIndex = treeInsert . flip zip [0..]

searchTree :: (Ord a,Eq a) => a -> Tree a -> Bool
searchTree x EmptyTree = False
searchTree x (Node y left right)
    | x == y = True
    | x < y = searchTree x left
    | otherwise = searchTree x right

searchTreeIndex :: (Ord a, Eq a) => a -> Tree (a,Int) -> Int
searchTreeIndex x EmptyTree = 0
searchTreeIndex x (Node y left right)
       | x == fst y = snd y
       | x < fst y = if left == EmptyTree then snd y else searchTreeIndex x left
       | x > fst y = if right == EmptyTree then snd y + 1 else searchTreeIndex x right
