import Data.List
validParentheses xs = null $ foldl' op [] xs
  where
    op ('(':xs) ')' = xs
    op xs x = x:xs
