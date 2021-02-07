import Data.List
import Data.Char

digits :: Int -> Int
digits = sum . map digitToInt . show

findAll s n = (l, x,y)
  where
    b = [x | x <- [10^(n-1)..10^n-1],show x == (sort $ show x)]
    a = [x | x <- b,digits x==s]
    l = length a
    x = if null a then Nothing else Just (a!!0)
    y = if null a then Nothing else Just (a!!(l-1))
