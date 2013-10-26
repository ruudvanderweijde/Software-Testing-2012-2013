module Assigment

where

import Data.List (sort, nub, insert, union)
import System.Random

--import Week4
--import STAL
import SetOrd
--import Hierarchy
-- import Chapter4
-- import Chapter5

{- Difficulties in chapter5: -}
-- In general, providing proof. Hard to find out HOW to prove things, what rules you need to supply to prove things.
-- I find it diffucult to understand composing relations. But I think that it will get better when I 'work' with it.
-- Restriction of exercise 5.52. I do not know what a restriction is.
-- Equivalence notation of modulo (like Proposition 5.65)
-- 5.6 and the rest... hard to understand the first time. Will have to re-read it.

{- Random data generator -}

-- Random data generator for datatype Set Int.
-- Time spent: 30 minutes.
randomSet :: IO (Set Int)
randomSet = do
        n <- getRandomInt 25
        m <- getRandomInt 25
        randomSet' n m

randomSet' :: Int -> Int -> IO (Set Int)
randomSet' 0 m = return (Set [])
randomSet' n m = do
        f <- getRandomInt m
        (Set fs) <- randomSet' (n-1) m
        --return (unionSet (Set [f]) fs)
        {- implementation without unionSet -}
        return (Set (nub (f:fs)))

randomSets :: Int -> IO [((Set Int),(Set Int))]
randomSets 0 = return []
randomSets n = do
        set1 <- randomSet
        set2 <- randomSet        
        sets <- randomSets (n-1)
        return ((set1,set2):sets)

noDuplicates :: Set Int -> Bool
noDuplicates (Set []) = True
noDuplicates (Set xs) = length xs == length (nub xs)

{- Test results:
   Run 100 tests for Unions
        *Assigment> testUnionSets
        "pass on:({5},{1,3,2})"
        "pass on:({0},{0,2,1})"
        "pass on:({0},{0})"
        "pass on:({0},{1,2,9})"
        *Assigment> *** Exception: failed test on:({2,5,6,1,8},{7,2,5,8})
        *Assigment> unionSet (Set [2,5,6,1,8]) (Set [7,2,5,8])
        {1,2,5,6,7,2,5,8}
   Debugging. The provided inSet function expects sorted Sets, updated the function to inSet'.
   After running many times 100 tests (using testSetFunctions), they all pass.
   Time spent, about 3 hours, mainly sorting out the test functions and debugging unionSet'.
-}

testSetFunctions :: IO ()
testSetFunctions = tests 100
        (\ set1 set2 -> 
                if (noDuplicates set1 
                 && noDuplicates set2
                 && noDuplicates (unionSet'     set1 set2)
                 && noDuplicates (intersectSet  set1 set2)
                 && noDuplicates (differenceSet set1 set2))
                then True
                else False
        )

tests :: Int -> (Set Int -> Set Int -> Bool) -> IO ()
tests n f = do 
  fs <- randomSets n
  test n f fs

test :: Int -> (Set Int -> Set Int -> Bool) -> [((Set Int),(Set Int))] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n f (x:xs) = 
  if f (fst(x)) (snd(x))
  then do print ("pass on:" ++ show x)
          test n f xs
  else error ("failed test on:" ++ show x)


{- unionSet is already defined in SetOrd.hs -}
-- unionSet' must not introduce duplicates
unionSet' :: Ord a => Set a -> Set a -> Set a
unionSet' (Set []) set = set
unionSet' (Set (x:xs)) set
        | inSet' x set = unionSet' (Set xs) set
        | otherwise    = unionSet' (Set xs) (insertSet x set)

-- intersectSet checks which elements of set1 occurs in set2
intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set []) _ = Set []
intersectSet (Set (x:xs)) set
        | inSet' x set = insertSet x (intersectSet (Set xs) set)
        | otherwise   = intersectSet (Set xs) set

-- differenceSet does (Set A - Set B)
differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet set (Set [])     = set
differenceSet set (Set (x:xs)) = differenceSet (deleteSet x set) (Set (xs))

-- inSet' checks wether an element exists in a Set
inSet' :: Ord a => a -> Set a -> Bool  
inSet' x (Set xs) = elem x xs



{- Binary relations -}
{- Time spent: about 3 hours -}

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- transitive Closure, keep on going till it is transitive.
trClos :: Ord a => Rel a -> Rel a
trClos r 
        | transR r  = r
        | otherwise = trClos (union r (compR r r))

-- Relation is transitive?
transR :: Ord a => Rel a -> Bool
transR [] = True
transR s  = and [ trans pair s | pair <- s ] 
        where trans (x,y) r = and [ elemInRel (x,v) r | (u,v) <- r, u == y ]

-- inSet' checks wether an element exists in a Set
elemInRel :: Ord a => (a,a) -> [(a,a)] -> Bool  
elemInRel (x,y) []      = False
elemInRel (x,y) (r:rs)
        | x == fst(r) && y == snd(r) = True
        | otherwise = elemInRel (x,y) rs

-- Composition of a relation
composePair :: Ord a => (a,a) -> Rel a -> Rel a
composePair (x,y) [] = []
composePair (x,y) ((u,v):s)
        | y == u = insert (x,v) (composePair (x,y) s)
        | otherwise = composePair (x,y) s

compR :: Ord a => Rel a -> Rel a -> Rel a
compR [] _        = []
compR ((x,y):s) r = union (composePair (x,y) r) (compR s r)

{- Test report:
        Testable properties:
        - No duplicate items.
        - No duplicate items after transitive closure.
        - Domain is the same as the original list.
        - Range is the same as the original list.
   Test Results:
        "50000 tests passed"

-}

testTR :: IO ()
testTR = testTRn 100

testTRn :: Int -> IO ()
testTRn n = tests' n
        (\ r -> 
                if (length r == length (removeDuplicates r)
                 && length (trClos r) == length (removeDuplicates (trClos r))
                 && domR(r) == domR(trClos r)
                 && ranR(r) == ranR(trClos r))
                then True
                else False
        )


tests' :: Int -> (Rel Int -> Bool) -> IO ()
tests' n f = do 
  fs <- randomRels n
  test' n f fs

test' :: Int -> (Rel Int -> Bool) -> [Rel Int] -> IO ()
test' n _ [] = print (show n ++ " tests passed")
test' n f (x:xs) = 
  if f x
  then do print ("pass on:" ++ show x)
          test' n f xs
  else error ("failed test on:" ++ show x)


randomRel :: IO (Rel Int)
randomRel = do
        n <- getRandomInt 25
        m <- getRandomInt 25
        randomRel' n m

-- this function just removes duplicate entries
randomRel' :: Int -> Int -> IO (Rel Int)
randomRel' n m = do
        x <-  (randomRel'' n m)
        return (removeDuplicates x)

randomRel'' :: Int -> Int -> IO (Rel Int)
randomRel'' 0 m = return []
randomRel'' n m = do
        x <- getRandomInt m
        y <- getRandomInt m
        rs <- randomRel'' (n-1) m
        return ((x,y):rs)

randomRels :: Int -> IO [Rel Int]
randomRels 0 = return []
randomRels n = do
        r  <- randomRel
        rs <- randomRels (n-1)
        return (r:rs)

removeDuplicates :: Ord a => Rel a -> Rel a
removeDuplicates []   = []
removeDuplicates (r:rs)
        | elemInRel r rs = removeDuplicates rs
        | otherwise      = insert r (removeDuplicates rs)

-- Domain of relation
domR :: Ord a => Rel a -> [a]
domR r = sort $ nub [ x | (x,_) <- r ]

-- Range of relation
ranR :: Ord a => Rel a -> [a]
ranR r = sort $ nub [ y | (_,y) <- r ]


{- Helper functions -}

-- getRandomInt creates a random integer between zero and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

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