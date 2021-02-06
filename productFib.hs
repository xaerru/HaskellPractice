productFib :: Integer -> (Integer, Integer, Bool)
productFib n = go 0 1 n
  where
    go a b c
        | a * b >= c = (a, b, a * b == c)
        | otherwise = go b (a + b) c
