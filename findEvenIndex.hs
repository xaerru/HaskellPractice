import Data.List
import Data.Maybe
splitAt' = \n -> \xs -> ((sum$take n xs)==(sum$drop (n+1) xs))
findEvenIndex a = if null r then (-1) else fromJust$r
  where
    r = elemIndex True [splitAt' n a|n<-[0..length a]]
