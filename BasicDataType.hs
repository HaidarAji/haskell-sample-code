module BasicDataType (
    ) where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

greetIfCool :: String -> String
greetIfCool coolness = 
    let cool = coolness == "this is very cool"
    in 
    if cool then "eyyyy. What's shakin?" else "pshhhhh"

greetIfCoolIO :: String -> IO()
greetIfCoolIO = putStrLn . greetIfCool

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome :: [String]
alsoAwesome = ["Quake", "The Simons"]
allAwesome :: [[String]]
allAwesome = [awesome, alsoAwesome]