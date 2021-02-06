import Data.Char
toWeirdCase :: String -> String
toWeirdCase = unwords.map (zipWith ($) $cycle [toUpper, toLower]).words
