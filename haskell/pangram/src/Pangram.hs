module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram text = countLetters (map (\x -> toLower x) text) == 26

countLetters :: [Char] -> Int
countLetters (x:xs) = if x `elem` xs || not (x `elem` ['a'..'z']) then
                        countLetters xs
                      else
                        1 + countLetters xs
countLetters [] = 0
