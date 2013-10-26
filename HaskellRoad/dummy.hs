
count :: Char -> String -> Int
count _ [] = 0
count c (x:xs)
        | c == x    = 1 + count c xs
        | otherwise = count c xs

--inf   ix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y