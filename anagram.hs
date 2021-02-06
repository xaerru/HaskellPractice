import Data.List
anagrams :: String -> [String] -> [String]
anagrams w ws = [x | x<-ws,sort x == sort w]
