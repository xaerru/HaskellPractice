import Data.List
ham = 1 : foldr u [] [2,3,5] 
    where
        u n s = 
                r where 
                r = merge s (map (n*) (1:r))
 
merge [] b = b
merge a@(x:xs) b@(y:ys) | x < y     = x : merge xs b
                        | otherwise = y : merge a ys
hamming n = ham!!(n-1)
