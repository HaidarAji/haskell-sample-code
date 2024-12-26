module HaskellProblems (
                       ) where

import Data.List
import Distribution.Simple.Utils (xargs)
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "Singleton List"
myButLast (x:[_]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Nothing"
elementAt [] _ = error "Out Of Length"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

elementAt' :: [a] -> Int -> Maybe a
elementAt' [] _ = Nothing
elementAt' (x:xs) 0 = Just x
elementAt' (x:xs) n = elementAt' xs (n - 1)

myLength :: [a] -> Int
myLength = foldr (\_ acc -> 1 + acc) 0

myReverse :: [a] -> [a]
myReverse xs = go xs []
   where go [] zs = zs
         go (y:ys) zs = go ys (y:zs)

isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome xs
    | xs == myReverse xs = True
    | otherwise = False

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress xs = go xs []
    where go [] zs = reverse zs
          go (y:ys) zs
              | y `elem` zs = go ys zs
              | otherwise = go ys (y:zs)

compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y = compress' ys
    | otherwise = x : compress' ys
compress' ys = ys

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack all@(x:xs) = takeWhile (==x) all : pack (dropWhile (==x) all)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x:xs)
    | f x = x : myTakeWhile f xs
    | otherwise = myTakeWhile f xs

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack' rest

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode xs = [ (a,b) | xs <- packed, let a = length xs, let b = head xs ]
    where packed = pack' xs

data Count a = None | Single a | Multiple Int a deriving Show

pairToCount :: (Int, a) -> Count a
pairToCount (0,_) = None
pairToCount (1,x) = Single x
pairToCount (n,x) = Multiple n x

encodeModified :: Eq a => [a] -> [Count a]
encodeModified = map pairToCount . encode

decode :: Eq a => [(Int,a)] -> [a]
decode = concatMap singleDecode
    where singleDecode (n,x) = replicate n x

decodeModified :: Eq a => [Count a] -> [a]
decodeModified = mconcat . map singleDecodeModified
    where singleDecodeModified (Single x) = [x]
          singleDecodeModified (Multiple n x) = replicate n x

decodeModified' :: Eq a => [Count a] -> [a]
decodeModified' [] = []
decodeModified' (None:xs) = decodeModified' xs
decodeModified' ((Single x):xs) = x : decodeModified' xs
decodeModified' ((Multiple 2 x):xs) = x : x : decodeModified' xs
decodeModified' ((Multiple n x):xs) = x : decodeModified' (Multiple (n-1) x:xs)

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where helper x [] = [(1,x)]
          helper x (all@(n,y):xs)
              | x == y = (n+1,y):xs
              | otherwise = (1,x):all:xs

dupli :: [a] -> [a]
dupli = concatMap . replicate $ 2

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:dupli xs

dupli'' :: [a] -> [a]
dupli'' = mconcat . map (replicate 2)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli xs 1 = xs
repli xs n = mconcat . map (replicate n) $ xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs 0 = xs
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n $ xs) n

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = helper xs n 1
    where helper [] _ _ = []
          helper ys 0 _ = ys
          helper (y:ys) n m
              | n == m = helper ys n 1
              | otherwise = y : helper ys n (m+1)

dropEvery'' :: [a] -> Int -> [a]
dropEvery'' xs n = helper xs n
    where helper [] _ = []
          helper (y:ys) 1 = helper ys n
          helper (y:ys) k = y : helper ys (k-1)

repli' :: [a] -> Int -> [a]
repli' xs n = xs >>= replicate n

split :: [a] -> Int -> ([a],[a])
split xs 0 = ([],xs)
split [] n = ([],[])
split xs n = (take n xs, drop n xs)

split' :: [a] -> Int -> ([a],[a])
split' [] _ = ([],[])
split' all@(x:xs) n
    | n > 0 = (x:ys,zs)
    | otherwise = ([],all)
      where (ys,zs) = split xs (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs from to = take ((to - from) + 1) . drop (from-1) $ xs

slice' :: [a] -> Int -> Int -> [a]
slice' xs f t = go 0 xs f t []
  where go _ [] _ _ zs = zs
        go n (y:ys) a b zs
            | a <= n && n <= b = go (n+1) ys a b (y:zs)
            | n > b = zs
            | otherwise = go (n+1) ys a b zs

slice'' :: [a] -> Int -> Int -> [a]
slice'' xs f t = go 0 xs f t
      where go _ [] _ _ = []
            go n (y:ys) a b
              | a <= n && n <= b = y : go (n+1) ys a b
              | n > b = []
              | otherwise = go (n+1) ys a b

rotate :: [a] -> Int -> [a]
rotate xs n
    | n == 0 = xs
    | n > 0 = drop n xs ++ take n xs
    | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

rotate' :: [a] -> Int -> [a]
rotate' xs 0 = xs
rotate' [] _ = []
rotate' xs n
    | n < 0 = gomin xs n []
    | n > 0 = gopos xs n []
     where gomin ys 0 zs = zs ++ ys
           gomin ys m zs = gomin (init ys) (m+1) (last ys : zs)
           gopos ys 0 zs = ys ++ zs
           gopos (y:ys) m zs = gopos ys (m-1) (zs++[y])

removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "out of scope"
removeAt 0 xs = error "null"
removeAt n xs = (take n xs !! max 0 (n-1), take (n-1) xs ++ drop n xs)

removeAt' :: Int -> [a] -> (a,[a])
removeAt' _ [] = error "out of scope"
removeAt' 0 xs = error "null"
removeAt' n xs = go n xs []
    where go 1 (y:ys) zs = (y,zs ++ ys)
          go n (y:ys) zs = go (n-1) ys (zs ++ [y])

removeKu:: Int -> [a] -> [a]
removeKu 0 xs = xs
removeKu 1 (x:xs) = xs
removeKu n (x:xs) = x : removeKu (n-1) xs

removeAt'' :: [a] -> ([a],[a])
removeAt'' [] = ([],[])
removeAt'' (x:xs) = ([x],xs)
