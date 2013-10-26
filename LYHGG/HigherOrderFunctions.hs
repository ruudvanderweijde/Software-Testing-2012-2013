module HigherOrderFunctions

where
	
--
-- Unknown chapter :)
--
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

--
-- Chapter: Higher order functions
--
chain :: Integral a => a -> [a]
chain 1 = [1]
chain x
	| x < 1 = error "invalid number input, number must be higher then 0"
	| odd x = x : chain(x*3+1)
	| even x = x : chain(div x 2)

numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100])) 
-- numLongChains = length (filter isLong (map chain [1..100]))  
--     where isLong xs = length xs > 15  

-- inline oneliner:
-- let isLong xs = length xs > 15 in length (filter isLong (map chain [1..100]))


-- usage of foldl and fold2 (loops through all elements of the list)
mapl' :: (a -> b) -> [a] -> [b]  
mapl' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

mapr' :: (a -> b) -> [a] -> [b]  
mapr' f xs = foldr (\x acc -> f x : acc) [] xs   

-- 'powerful' fold implementations
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

-- In the end, we build up a reversed list. 
-- \acc x -> x : acc kind of looks like the : function, only the parameters are flipped. 
-- That's why we could have also written our reverse as foldl (flip (:)) [].
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- scanl && scanr
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  

-- $ is called function application
--
-- Most of the time, it's a convenience function so that we don't 
-- have to write so many parentheses. Consider the expression 
-- 	sum (map sqrt [1..130]). 
-- Because $ has such a low precedence, we can rewrite that expression as 
-- 	sum $ map sqrt [1..130], 
-- saving ourselves precious keystrokes! 

-- . function composition
--
-- its like f . g x == f(g(x))
-- Examples:
-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 
--  <=>
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 
--
-- The expression f (g (z x)) is equivalent to (f . g . z) x. 
--
-- sum (replicate 5 (max 6.7 8.9)) 
-- (sum . replicate 5 . max 6.7) 8.9 
-- sum . replicate 5 . max 6.7 $ 8.9


-- In the section about maps and filters, we solved a problem of finding the sum of all odd squares that are smaller than 10,000. Here's what the solution looks like when put into a function.

oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))     
-- Being such a fan of function composition, I would have probably written that like this:

oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  
-- However, if there was a chance of someone else reading that code, I would have written it like this:

oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  

    