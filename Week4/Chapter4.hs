module Chapter4 

where

import Week4
import STAL
import SetEq
import Hierarchy

delete :: Eq a => a -> [a] -> [a]
delete x []         = []
delete x (y:ys)
        | x == y    = ys
        | otherwise = y : delete x ys

union :: Eq a => [a] -> [a] -> [a]
union [] ys     = ys
union (x:xs) ys = x : union xs (delete x ys)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _      = []
intersect (x:xs) s 
        | elem x s  = x : intersect xs s
        | otherwise = intersect xs s

-- HR Book exercise 4.51
difference :: Eq a => [a] -> [a] -> [a]
difference [] _    = []
difference (x:xs) s
        | elem x s  = difference xs s
        | otherwise = x : difference xs s

notElem :: Eq a => a -> [a] -> Bool
notElem = all . (/=)

-- HR Book exercise 4.53 
genUnion :: Eq a => [[a]] -> [a]
genUnion []     = []
genUnion (x:xs) = union x (genUnion xs)

genIntersect :: Eq a => [[a]] -> [a]
genIntersect []     = []
genIntersect (x:xs) 
        | null xs   = x 
        | otherwise = intersect x (genIntersect xs)

-- HR Book exercise 4.54
unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set []) (Set set)     = Set set
unionSet (Set (x:xs)) (Set set) = insertSet x (unionSet (Set xs) (deleteSet x (Set set)))

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set []) _      = Set []
intersectSet (Set (x:xs)) set
        | inSet x set = insertSet x (intersectSet (Set xs) set)
        | otherwise   = intersectSet (Set xs) set

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set []) _    = Set []
differenceSet (Set (x:xs)) set
        | inSet x set = differenceSet (Set xs) set
        | otherwise   = insertSet x (differenceSet (Set xs) set)

-- HR Book exercise 4.55
-- I have no idea what needs to be changed, besides a pre-/post-condition maybe?



