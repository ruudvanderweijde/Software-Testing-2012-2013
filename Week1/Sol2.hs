module Week1.Sol2  

where 

import Week1.GS
import Week1.TAMO

-- Exercise 2.2 (+2.4) (+2.9)
--
-- Question:	Make up the truth table for the exclusive version of or.
--
-- Answer:		+---+-------++-------+----------++----------------+
--				|2.2|  2.2  ||  2.4  |   2.4    ||       2.9      |
--				+---+-------++-------+----------++----------------+
--				|P Q|P XOR Q||P <=> Q|!(P <=> Q)||(P XOR Q) XOR Q)|
--				+---+-------++-------+----------++----------------+
--				|t t|   f   ||   t   |    f     ||        t       |
--				|t f|   t   ||   f   |    t     ||        t       |
--				|f t|   t   ||   f   |    t     ||        f       |
--				|f f|   f   ||   t   |    f     ||        f       |
--				+---+-------++-------+----------++----------------+

-- Exercise 2.4
--
-- Question:	Check that the truth table for exclusive or from Exercise 2.2 is equivalent
--				to the table for !(P <=> Q). Conclude that the Haskell implementation of the
--				function <+> for exclusive or in the frame below is correct.
--
-- Answer:		See exercise 2.2 for the truth table.
--				The Haskell function is implemented to return true if x and y are not the same.
--				This confirms the truth table.

-- Exercise 2.9
--
-- Question:	Let  stand for exclusive or. Show, using the truth table from Exercise
--				2.2, that (P  Q)  Q is equivalent to P.
--
-- Answer:		See exercise 2.2 for the truth table.

-- Exercise 2.13
test_1a = not True <=> False 
test_1b = not False <=> True 
test_2  = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
test_3a = logEquiv1 (\ p -> p || True) (const True)
test_3b = logEquiv1 (\ p -> p && False) (const False)
test_4a = logEquiv1 (\ p -> p || False) id
test_4b = logEquiv1 (\ p -> p && True) id
test_5  = logEquiv1 (\ p -> p || not p) (const True)
test_6  = logEquiv1 (\ p -> p && not p) (const False)

-- Exercise 2.15
--contradiction1 ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
--contradiction1 bf1 bf2 =  
--    (bf1 True) && (bf2 False) 

--contradiction2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
--contradiction2 bf1 bf2 = 
--  and [(bf1 p q) <=> (bf2 p q)  |  p <- [True,False], 
--                                   q <- [True,False]]

--contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
--contradiction3 bf1 bf2 = 
--  and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True,False], 
--                                      q <- [True,False], 
--                                      r <- [True,False]] 

-- Exercise 2.16
-- Question:	Produce useful denials for every sentence of Exercise 2.31. (A denial
--				of  is an equivalent of :.)
--
-- Answer:		1. The equation x2 + 1 = 0 has NOT a solution.
--				2. A largest natural number does exist.
--				3. The number 13 is not prime (use djn for ‘d divides n’).
--				4. The number n is not prime.
--				5. There are not infinitely many primes


-- Exercise 2.17
-- Answer:		x = 3, y = 5, z = 1


-- Exercise 2.18
-- Answer:		False <=> False is equivalent to (not False) <=> (not False)
--				not False <=> False is equivalent to False <=> not False

-- Exercise 2.19
-- Answer:		Hard to explain...

-- Exercise 2.20
-- Answer:		3, 4 and 6.

-- Exercise 2.21 
-- Answer:		1: p || q ==> p 	(p V q → p)
--				2: There are 2*2*2*2=16 truth tables for 2-letter formulas.
--				3: Yes, I think you can find formulas for all of them.
--				4: I'm not sure, but I guess someone has them.
--				5: I think the same answers apply to the other 2+-letter formulas.

-- Exercise 2.22
-- Answer:		Because the list of usable numbers is infinite, you can always
--				find a number between the numbers.

-- Exercise 2.23, 2.26, 2.27, 2.31, 2.32, 2.33, 2.34, 2.35 
-- Answer:		Written down in textbook. Hard to do this in the editor.
-- Difficulties 2.31: Not sure what the exact meaning is, feels ambigious for me.
-- Difficulties 2:32: Not sure about the last one (#4).
-- Difficulties 2:34: Hard to translate, I need more practise.

-- Exercise 2.36
-- Answer:		1: There is a rationalnumber which has a squareroot of 5.
--				2: For every natural number there is a higher natural number.
--				3: For every natural number there is no number which is bigger 
--					than one and smaller than 2n + 1 and 2n + 1 at the same time. (??)
--				4: ??

-- Exercise 2.37
-- Answer:		1: [f,f,f,f,f,f]
--				2: [t,t,t,t,t,f]
--				3: [t,t,f,t,f,f]
--				4: [f,t,t,t,f,t]
--				5: [f,f,f,f,f,f]

-- Exercise 2.38, 2.39, 2.41, 2.46, 2.47, 2.48, 2.50
-- Lack of time to execute this exercise.

-- Exercise 2.39
-- (??) I dont know how to do this.

-- Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- Exercise 2.52
parity :: [Bool] -> Bool
parity [] = True
parity xs = even (length (filter (==True) xs))

-- Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = True -- TODO 
