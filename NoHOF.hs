module NoHOF ( mapIncrmt
             , mapDouble ) where

incrmt :: Num a => a -> a
incrmt = (+1)

mapIncrmt :: Num a => [a] -> [a]
mapIncrmt xs = [ incrmt x | x <- xs ]

double :: Num a => a -> a
double = (*2)

mapDouble :: Num a => [a] -> [a]
mapDouble [] = []
mapDouble (x:xs) = double x : mapDouble xs