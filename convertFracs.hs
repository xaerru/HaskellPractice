type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = zip f (take (length xs) (repeat r))
  where
    e = unzip xs
    r = lcmm $ snd e
    f = map (\x->fst x*r`div`snd x) xs
