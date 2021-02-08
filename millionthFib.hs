import Control.Arrow ((&&&))
import Data.Bits
 
fibstep :: (Integer, Integer) -> (Integer, Integer)
fibstep (a, b) = (b, a + b)
 
fibnums :: [Integer]
fibnums = map fst $ iterate fibstep (0, 1)
 
fibN2 :: Integer -> (Integer, Integer)
fibN2 m
  | m < 10 = iterate fibstep (0, 1) !! fromIntegral m
fibN2 m = fibN2_next (n, r) (fibN2 n)
  where
    (n, r) = quotRem m 3
 
fibN2_next (n, r) (f, g)
  | r == 0 = (a, b) 
  | r == 1 = (b, c)
  | r == 2 = (c, d)
  where
    a =
      5 * f ^ 3 +
      if even n
        then 3 * f
        else (-3 * f)
    b = g ^ 3 + 3 * g * f ^ 2 - f ^ 3
    c = g ^ 3 + 3 * g ^ 2 * f + f ^ 3
    d =
      5 * g ^ 3 +
      if even n
        then (-3 * g)
        else 3 * g
 
fib n
  | n>=0 = fst $ fibN2 n
  | otherwise = (fst $ fibN2 (n*(-1)))*(if n`mod`2==1 then (1) else (-1))
