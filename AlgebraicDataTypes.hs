module AlgebraicDataTypes where

data PugType = PugData
data HuskyType a = HuskyData
newtype DogueDeBordeaux doge = DogueDeBordeaux
    { getDoge :: doge }

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [String]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux "Arci"

data Doggies a =
    Husky a
    | Mastiff a deriving (Eq, Show)


instance Show a => Show (DogueDeBordeaux a) where
    show :: Show a => DogueDeBordeaux a -> String
    show (DogueDeBordeaux x) = "Doge: " <> show x

data Mood = Blah | Woot deriving (Show, Eq)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
