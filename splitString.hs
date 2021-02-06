import Data.List
solution :: String -> [String]
solution [] = []
solution (x:[]) = [[x,'_']]
solution (x:y:xs) = [x,y]:(solution xs)
