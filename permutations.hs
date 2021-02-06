has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs
rotations :: [a] -> [[a]]
rotations xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (rotations.(x:)) (perms xs)
permutations :: String -> [String]
permutations a = unique $ perms a

-- Alternate Solution

import Data.List (delete, nub)

permutations :: String -> [String]
permutations "" = [""]
permutations xs = [x : y | x <- nub xs, y <- permutations $ delete x xs]
