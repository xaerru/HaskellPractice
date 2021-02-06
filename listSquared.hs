is_square n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)

divisors n = is_square $ a
  where
    a = sum[x^2 | x <- [1..n], n `mod` x == 0]
help n = sum[x^2 | x <- [1..n], n `mod` x == 0]
listSquared m n = zip a [help x | x <- a] 
  where a=[x | x <- [m..n], divisors x]
