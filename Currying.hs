module Currying (
    ) where

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunc :: Integer -> Bool -> Integer
typicalCurriedFunc i = (+) i . nonsense

uncurriedFunc :: (Integer, Bool) -> Integer
uncurriedFunc (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + nonsense b