import Data.Char
import Data.List
import Data.Maybe
high "" = ""
high myStr = a!!(fromJust $ elemIndex c b)
  where
    op = sum . map (\x -> (ord x)-96)
    a  = words myStr
    b = map op $ a
    c = maximum b
