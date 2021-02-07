fusc i
  | 1 > i = 0
  | otherwise = fst $ help (pred i)
  where
    help n
      | 0 == n = (1, 0)
      | even n = (x + y, y)
      | otherwise = (x, x + y)
      where
        (x, y) = help (n `div` 2)
