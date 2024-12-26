module PatternMatching (
    ) where

newtype Username = Username String
newtype AccountNumber = AccountNumber {getAccNum :: Integer}

data User = UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name)
                            (AccountNumber accNum))
                            = putStrLn $ name <> " " <> show accNum
                    
data WherePenguinsLive =
    Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica deriving (Eq, Show)

newtype Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfricaPeng :: WherePenguinsLive -> Bool
isSouthAfricaPeng SouthAfrica = True
isSouthAfricaPeng _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

