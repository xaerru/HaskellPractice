import Data.List
import Data.Char
import Data.Maybe
nextSmaller n = if (fromJust i-1)<0 then Nothing else Just (f!!(fromJust i-1))
  where
    r = map intToDigit $ map digitToInt (show n)
    l = length$show n
    f = filter (\x->(length$show x)==l)$ sort $ map (\x->read x::Integer) (permutations $ r)
    i = elemIndex n f

