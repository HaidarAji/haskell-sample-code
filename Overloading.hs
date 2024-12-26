module Overloading ( ) where

data Option a = None | Some a

instance Eq a => Eq (Option a) where
  (==) None None = True
  (==) (Some x) (Some y) = x == y
  (==) _ _ = False


