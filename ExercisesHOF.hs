module ExercisesHOF ( mFilterMap
                    , mFilterMap'
                    , mAll
                    , mAny
                    , mTakeWhile
                    , mTakeWhile'
                    , mDropWhile
                    , mFilter
                    , twice
                    , map'
                    ) where
import Control.Monad (when)

mFilterMap :: (Ord a, Eq a) => (a -> Bool) -> (a -> b) -> [a] -> [b]
mFilterMap p f xs = [ f x | x <- xs, p x ]

mFilterMap' :: (Ord a, Eq a) => (a -> Bool) -> (a -> b) -> [a] -> [b]
mFilterMap' p f = map f . mFilter p

mAll :: (a -> Bool) -> [a] -> Bool
mAll p = foldr ((&&) . p) True

mAny :: (a -> Bool) -> [a] -> Bool
mAny = (or .) . map

mTakeWhile :: (a -> Bool) -> [a] -> [a]
mTakeWhile p xs = go p xs []
    where go _ [] zs = zs
          go f (y:ys) zs
            | f y = go f ys (y:zs)
            | otherwise = zs

mTakeWhile' :: (a -> Bool) -> [a] -> [a]
mTakeWhile' _ [] = []
mTakeWhile' p (x:xs)
    | p x = x : mTakeWhile' p xs
    | otherwise = []

mDropWhile :: (a -> Bool) -> [a] -> [a]
mDropWhile _ [] = []
mDropWhile p all@(x:xs)
    | p x = mDropWhile p xs
    | otherwise = all

mEven :: (Integral a) => a -> Bool
mEven = even

mOdd :: (Integral a) => a -> Bool
mOdd = not . mEven

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter _ [] = []
mFilter pred (x:xs)
    | pred x = x : mFilter pred xs
    | otherwise = mFilter pred xs

twice :: (a -> a) -> a -> a
twice f = f . f

map' :: (a -> b) -> [a] -> [b]
map' f xs = [ y | x <- xs, let y = f x ]