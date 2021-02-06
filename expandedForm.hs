import Data.List
expandedForm :: Int -> String
expandedForm = intercalate " + " . map(\(n, c) ->  c : replicate n '0' ) . reverse . filter ((/='0') . snd) . zip [0..] . reverse . show

