module Blog (
            ) where
import GHC.Integer.GMP.Internals (bigNatToInteger)
import Data.Monoid
import Control.Monad.List (when)
import Debug.Trace

mapIntegral :: Integral a => (a -> a) -> [a] -> [a]
mapIntegral _f [] = []
mapIntegral f (x:xs) = f x : mapIntegral f xs

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

qsort :: (Ord a, Eq a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ (x : qsort bigger)
    where smaller = filter (<x) xs
          bigger = filter (>=x) xs

sumList :: Num a => [a] -> a
sumList = getSum . mconcat . map Sum

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f s = case f s of
                Nothing -> []
                Just (a, s') -> a : unfoldr f s'

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' lo hi =
    unfoldr
      (\cur -> if cur > hi then Nothing else Just (cur, cur + 1))
      lo

fibs :: [Int]
fibs =
    unfoldr
      (\ (x, y) -> Just (y, (y, x+y)))
      (0, 1)

facts :: [Int]
facts =
    unfoldr
      (\(x, y) -> traceShow (x,y) $ Just (x, (x * (y+1), y+1)))
      (1,1)

fact :: Int -> Int
fact = getProduct
       . mconcat
       . unfoldr
          (\v -> if v == 0 then Nothing else Just (Product v, v - 1))
