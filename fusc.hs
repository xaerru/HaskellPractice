fusc 0 = 0
fusc 1 = 1
fusc n
  | n `mod` 2 == 0 = fusc (n`div`2)
  | otherwise = (fusc ((n-1)`div`2)) + (fusc ((n+1)`div`2))
