module Triangle 

where

import Data.List

data Shape 
	= NoTriangle	-- invalid triangle 
	| Equilateral	-- 3 sides of the same lenght
	| Isosceles 	-- 2 sides of the same lenght
	| Rectangular	-- is a pythagorean triangle 
	| Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' . sort $ [a,b,c]

triangle' :: [Integer] -> Shape
triangle' [a,b,c]
	| isInvalid = NoTriangle 
	| allAreEqual = Equilateral 
	| twoAreEqual = Isosceles 
	| isPythagorean = Rectangular
	| otherwise = Other
	where
		positiveInts = all (>0) [a,b,c]
		isInvalid = not positiveInts || (a + b <= c) 
		allAreEqual = length (nub([a, b, c])) == 1
		twoAreEqual = length (nub([a, b, c])) == 2
		isPythagorean = a^2 + b^2 == c^2