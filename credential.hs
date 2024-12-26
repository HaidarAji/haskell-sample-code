import Text.ParserCombinators.Parsec
--import Data.String.CSV

--main = mapM_ putStrLn $ tellCredential email
--main = do
--       result <- parseFromFile csvFile "list-credentials.csv"
--       case result of
--          Left err -> print err
--          Right (x:xs) -> 

data Credential = Credential { ssid :: [Char]
                             , site :: [Char]
                             , user :: [Char]
                             , password :: [Char] } deriving Show

email :: Credential
email = Credential { ssid="Email"
                   , site="Work"
                   , user="radenhaidar.prabowo@gmail.com"
                   , password="Maniac351463@" }

tellCredential :: Credential -> [String]
tellCredential (Credential i s u p) = ["Id: " ++ i
                                      ,"Instance: " ++ s
                                      ,"User: " ++ u
                                      ,"Password: " ++ p
                                      ,"-----------------------------"]

encodeCredential :: [String] -> Credential
encodeCredential [] = Credential { ssid=""
                                 , site=""
                                 , user=""
                                 , password="" }
encodeCredential [i,s,u,p] =  Credential { ssid=i
                                 , site=s
                                 , user=u
                                 , password=p }
 
 
