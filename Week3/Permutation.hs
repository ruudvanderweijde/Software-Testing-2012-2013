module Permutation

where 

import Week2
import Data.List
import Data.Char
import System.Random
import Week3
import Techniques

-- genIntList creates a list of max 8 Ints from zero to 100
genIntList :: IO [Int]
genIntList = do 
        n <- getRandomInt 8
        m <- getRandomInt 100
        getRandomInts n m 
                   
-- getRandomInts creates a list of 'n' Ints from zero to 'm'
getRandomInts :: Int -> Int -> IO [Int]
getRandomInts 0 m = return []
getRandomInts n m = do
        f <- getRandomInt m
        fs <- getRandomInts (n-1) m
        return (f:fs)

-- isPermutation compares two lists and returns True if the list is a permutation
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []             = True
isPermutation [] _              = False
isPermutation _ []              = False
isPermutation (x:xs) (y:ys)  
        | x == y = isPermutation xs ys
        | otherwise = isPermutation xs (delete x (y:ys))

-- Exercise 5: Define some testable properties for isPermutation.
-- 
-- A permutation is of the same length.
--      Testable: length xs == length ys
-- A permutation contains the same numbers but may appear in different order, 
-- so the ordered lists of the items are the same.
--      Testable: sort xs == sort ys
-- The sum of a permutation is the same.
--      Testable: sum xs == sum ys
--
-- A permutation is invalid if the first item of the generated list.
--      Testable: drop 1 xs /= ys
-- A permutation is invalid if we add an item to the generated list.
--      Testable: xs++[1] /= ys
--
-- All these properties are included in the test below
-- Time spent: about 2 hours

testPermutations :: [Int] -> Bool
testPermutations [] = error "empty list supplied."
testPermutations xs = all (\x -> isPermutation x xs) (permutations xs)
        && all (\x -> length x == length xs) (permutations xs)
        && all (\x -> sort x == sort xs) (permutations xs)
        && all (\x -> sum x == sum xs) (permutations xs)
        && not (any (\x -> isPermutation (drop 1 x) xs) (permutations xs))
        && not (any (\x -> isPermutation (x++[1]) xs) (permutations xs))

test1 = all 
{-
-- Test output:
        *Assigments> testPermutations [0,0,0]
        True
        *Assigments> genIntList
        [58,349,125]
        *Assigments> testPermutations [58,349,125]
        True
        *Assigments> genIntList
        [380,385,766,621,694,348,798]
        *Assigments> testPermutations [380,385,766,621,694,348,798]
        True
        *Assigments> genIntList
        []
        *Assigments> testPermutations []
        *** Exception: empty list supplied.
        *Assigments> genIntList
        []
        *Assigments> genIntList
        [64,20,26,48,45]
        *Assigments> testPermutations [64,20,26,48,45]
        True
        *Assigments> all testPermutations [[x,y,z] | x <- [0..10], y <- [0..10], z <- [0..10]]
        True
-}