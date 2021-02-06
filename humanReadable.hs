import Text.Printf
humanReadable :: Int -> String
humanReadable x = printf "%02d:%02d:%02d" hours minutes (x `mod` 60)
  where
    hours = (x`div`3600)
    minutes = ((x`mod`3600)`div`60)
