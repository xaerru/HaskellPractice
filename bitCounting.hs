countBits :: Int -> Int
countBits 0 = 0
countBits n = let (d,r) = n `divMod` 2
         in r + countBits d
