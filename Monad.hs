module Monad (
             ) where

import Control.Monad () 
import Data.Monoid ( Sum(Sum) )
safeDiv :: Int -> Int -> Maybe Int
safeDiv _m 0 = Nothing
safeDiv m n = Just (m `div` n)

numbered :: Char -> (Sum Int, Char)
numbered x = (Sum 1, x)
