{-# LANGUAGE UndecidableInstances #-}
module Monoid (
              ) where
import qualified Data.Foldable as F
import Distribution.Simple (KnownExtension(NamedWildCards))
import Data.Monoid
import Control.Monad.ST (ST)

newtype IProduct a = IProduct { getIProduct :: a }
                deriving (Show, Eq, Ord, Num)

instance Num a => Semigroup (IProduct a) where
    (IProduct m) <> (IProduct n) = IProduct (m * n)

instance (Num a, Foldable ((->) ([IProduct a] -> IProduct a))) => Monoid (IProduct a) where
    mempty = IProduct 1
    mappend = (<>)
    mconcat = F.fold mappend mempty

data Optional a = 
      Nada
    | Only a deriving (Eq, Show)

instance Semigroup s => Semigroup (Optional s) where
    Nada <> _ = Nada
    (Only m) <> (Only n) = Only (m <> n)

instance Monoid m => Monoid (Optional m) where
    mempty = Nada
    mappend = (<>)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Excalmation = String

madlibbin' :: Excalmation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbin' e adv noun adj =
    mconcat [e, "! he said "
            , adv, " as he jumped into his car "
            , noun, " and drove off with this "
            , adj, " wife."]