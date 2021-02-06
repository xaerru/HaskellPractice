import Data.Char (intToDigit, digitToInt)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = show ((read xs :: Integer)*(read ys :: Integer))
