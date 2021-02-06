import Data.Char
wave :: String -> [String]
wave xs = [take x xs ++ toUpper (xs!!x) : drop (x+1)xs | x <- [0..length xs - 1], xs!!x /= ' ']
