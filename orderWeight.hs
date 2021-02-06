import Data.Char 
import Data.List 
import Data.Ord  

orderWeight :: [Char] -> [Char]
orderWeight = unwords . sortBy (comparing weight) . words
  where weight s = (sum . map digitToInt $ s, s)
