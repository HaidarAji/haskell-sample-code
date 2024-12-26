module Polymorphic (
    ) where
import AlgebraicDataTypes (badDoge)

kCom :: a -> b -> a
kCom x _ = x

kCom2 :: a -> b -> b
kCom2 _ y = y

sCom :: (a -> b -> c) -> (a -> b) ->  a -> c
sCom f g x = f x . g $ x