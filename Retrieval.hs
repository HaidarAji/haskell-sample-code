module Retrieval (
                 ) where

{- this module's purpose is for remembering
 - everything's been learned before
 - so you might find the content of this module
 - redudant from any other module -}
fibonacci :: Int -> Int
fibonacci n = let go 0 (x,_) = x
                  go 1 (_,y) = y
                  go m (x,y) = go (m-1) (y,x+y)
              in go n (0,1)
                

fibonacciLazy :: Int -> Int
fibonacciLazy n 
      | n < 0 = 0
      | otherwise = last . take (n+1) $ listFib
          where listFib :: [Int]
                listFib = 0:1:(zipWith (+) listFib (tail listFib))
