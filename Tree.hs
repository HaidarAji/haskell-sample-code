module Tree (
            ) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

singletonTree :: (Ord a, Eq a) => a -> Tree a
singletonTree x = Node x (EmptyTree) (EmptyTree)

insertTree :: (Ord a, Eq a) => [a] -> Tree a
insertTree [] = EmptyTree
insertTree [x] = singletonTree x
insertTree xs = Node pivot (insertTree less) (insertTree greater)
    where pivot
             | even (length xs) = (!!) xs $ (length xs `div` 2) - 1
             | otherwise = (!!) xs $ length xs `div` 2
          less = filter (<pivot) xs
          greater = filter (>pivot) xs

searchTree :: (Ord a, Eq a) => a -> Tree a -> Bool
searchTree _ EmptyTree = False
searchTree x (Node y left right)
         | x == y = True
         | x < y = searchTree x left
         | otherwise = searchTree x right
