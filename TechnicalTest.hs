module TechnicalTest(
        ) where
import Data.Char ( toLower
                 , isAlphaNum )
import Data.Time.Calendar.Easter (gregorianEaster)
import Distribution.PackageDescription (TestSuiteInterface(TestSuiteUnsupported))

removeChar :: Char -> String -> String
removeChar c = filter (/= c)

isPalindrome :: String -> Bool
isPalindrome str = normalizeStr str == reverse (normalizeStr str)
    where normalizeStr = map toLower . filter isAlphaNum

nonRepeatedChar :: String -> Maybe Char
nonRepeatedChar [] = Nothing
nonRepeatedChar [x] = Just x
nonRepeatedChar all@(x:xs)
   | countChar x all == 1 = Just x
   | otherwise = nonRepeatedChar . removeChar x $ all

countChar :: (Foldable t, Eq a, Num b) => a -> t a -> b
countChar c = foldr (\z acc -> if c == z then acc + 1 else acc) 0

naiveSort :: (Ord a, Eq a) => [a] -> [a]
naiveSort [] = []
naiveSort (x:xs) = naiveSort lesser ++ x : naiveSort greater
    where lesser = filter (< x) xs
          greater = filter (>= x) xs

reverseNaiveSort :: (Ord a, Eq a) => [a] -> [a]
reverseNaiveSort = reverse . naiveSort

secondHighest :: [Integer] -> Maybe Integer
secondHighest [] = Nothing
secondHighest [x] = Nothing
secondHighest xs = Just . last . take 2 $ sorted
    where sorted = reverseNaiveSort xs