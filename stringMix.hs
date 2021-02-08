import Data.List
import Data.Char
import Control.Arrow

zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithDefault da db la lb = let len = max (length la) (length lb)
                                 la' = la ++ (repeat da)
                                 lb' = lb ++ (repeat db)
                             in take len $ zip la' lb'  

help s = (map (head &&& length) . group) [x | x <- s, isLower x]
a="my&friend&Paul has heavy hats! &"
b="my friend John has many many friends &"

f a = (fst r, snd r)
  where
    r = maximum [fst a,snd a]

mix s1 s2 = map f a
  where
    a = zipWithDefault ('#',0) ('#',0) (help s1) (help s2)

