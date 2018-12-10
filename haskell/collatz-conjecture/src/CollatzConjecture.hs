module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz initial
        | initial <= 0 = Nothing
        | otherwise = Just (step initial)

step :: Integer -> Integer
step 1 = 0
step num = if even num then
             1 + step (num `div` 2)
           else
             1 + step (num * 3 + 1)
