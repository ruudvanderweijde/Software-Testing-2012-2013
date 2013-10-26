module Week1.Sol1  

where 

import Week1.GS

-- Exercise 1.4
--
-- Question:	Suppose in the definition of ldf we replace the condition k^2 > n
-- 				by k^2 >= n, where >= expresses ‘greater than or equal’. Would that make any
-- 				difference to the meaning of the program? Why (not)?
--
-- Answer:		TODO

-- Exercise 1.6
-- 
-- Question:	Can you gather from the definition of divides what the type declaration
--				for rem would look like?
--
-- Answer:		TODO

-- Exercise 1.9
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

-- Exercise 1.10
removeFst :: (Eq a) => a -> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) 
	| x == y = ys
	| otherwise = y : (removeFst x ys)

-- Exercise 1.13
count :: Char -> String -> Int
count x [] = 0 
count x (y:ys) 
	| x == y = count x ys + 1 
	| otherwise = count x ys

-- Exercise 1.14
blowUp :: String -> String
blowUp [] = []
blowUp xs = blowUp' xs 1

blowUp' :: String -> Int -> String
blowUp' [] _ = []
blowUp' (x:xs) n = (replicate n x) ++ blowUp' xs (n+1)

-- Exercise 1.15
srtStrings :: [String] -> [String]
srtStrings [] = []
srtStrings xs = m : (srtStrings (removeFst m xs)) where m = mnmString xs

mnmString :: [String] -> String
mnmString [] = error "empty list"
mnmString [x] = x
mnmString (x:xs) = min x (mnmString xs)

-- Exercise 1.17
subString :: String -> String -> Bool
subString [] ys = True
subString (x:xs) [] = False
subString (x:xs) (y:ys) = (x==y) && (prefix' xs ys)
						|| (subString (x:xs) ys) 

prefix' :: String -> String -> Bool
prefix' [] ys = True
prefix' (x:xs) [] = False
prefix' (x:xs) (y:ys) = (x==y) && prefix' xs ys

-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths = map length

-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths (x:xs) = length x + sumLengths xs

-- Exercise 1.24
--
-- Question:	What happens when you modify the defining equation of ldp as
--				follows:
--					ldp :: Integer -> Integer
--					ldp = ldpf primes1
--				Can you explain?
--
-- Answer:		TODO