import Data.List

data Direction = North | East | West | South deriving (Eq, Show)

dirReduce :: [Direction] -> [Direction]
dirReduce = foldr collapse []

collapse North (South:xs) = xs
collapse South (North:xs) = xs
collapse East (West:xs) = xs
collapse West (East:xs) = xs
collapse x xs = x:xs
