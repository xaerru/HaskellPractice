import Data.Char
narcissistic n = (sum $ map (^l) $ digits n) == n
  where
    l = length $ show n
    digits n = [toInteger (digitToInt x) | x <- show n]
