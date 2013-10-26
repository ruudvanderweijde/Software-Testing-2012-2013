module Chapter5

where

import Data.List
import SetOrd

-- Difficulties in chapter5:
-- In general, providing proof. Hard to find out HOW to prove things, what rules you need to supply to prove things.
-- I find it diffucult to understand composing relations. But I think that it will get better when I 'work' with it.
-- Restriction of exercise 5.52. I do not know what a restriction is.
-- Equivalence notation of modulo (like Proposition 5.65)
-- 5.6 and the rest... hard to understand the first time. Will have to re-read it.

{-- Chapter 5.3 --}

type Rel a = Set (a,a)

-- Domain of relation
domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [ x | (x,_) <- r ]

-- Range of relation
ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [ y | (_,y) <- r ]

-- Identity relation Delta[A]
idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs]

-- Total relation over a set
totalR :: Ord a => Set a -> Rel a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs]

-- Invert relation
invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

-- Pair is in relation?
inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r

-- The complement of a relation
complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r = Set [(x,y) | x <- xs, y <- xs, not (inR r (x,y))]

-- Relation is reflexive?
reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = subSet (idR set) r

-- Relation is irreflexive?
irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r = all (\ pair -> not (inR r pair)) [(x,x) | x <- xs]

-- Relation is symmetric?
symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x,y):pairs)) 
        | x == y = symR (Set pairs)
        | otherwise = inSet (y,x) (Set pairs) && symR (deleteSet (y,x) (Set pairs))

-- Relation is transitive?
transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s)  = and [ trans pair (Set s) | pair <- s ] 
        where trans (x,y) (Set r) = and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]


-- Composition of a relation
composePair :: Ord a => (a,a) -> Rel a -> Rel a
composePair (x,y) (Set []) = Set []
composePair (x,y) (Set ((u,v):s))
        | y == u = insertSet (x,v) (composePair (x,y) (Set s))
        | otherwise = composePair (x,y) (Set s)

-- Union of two sets
{- already defined in SetOrd
unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 = insertSet x (unionSet (Set xs) (deleteSet x set2))
-}

-- Composition #2 in terms of compose Pair and unionSet
compR :: Ord a => Rel a -> Rel a -> Rel a
compR (Set []) _ = (Set [])
compR (Set ((x,y):s)) r = unionSet (composePair (x,y) r) (compR (Set s) r)

-- Composition of a relation with itself (Rn):
repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n 
        | n < 1 = error "argument < 1"
        | n == 1 = r
        | otherwise = compR r (repeatR r (n-1))

-- Example 5.51 Let us use the implementation to illustrate Exercise 5.39.
r = Set [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
r2 = compR r r
r3 = repeatR r 3
r4 = repeatR r 4       

s = Set [(0,0),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,2),(2,3)]
test = (unionSet r (compR s r)) == s

-- Exercise 5.52 
-- restrictR :: Ord a => Set a -> Rel a -> Rel a

-- Exercise 5.54
rclosR :: Ord a => Rel a -> Rel a
rclosR r = unionSet r (idR (unionSet (domR r) (ranR r)))


{-- Chapter 5.4 --}

divides :: Integer -> Integer -> Bool
divides d n 
        | d == 0    = error "divides: zero divisor"
        | otherwise = (rem n d) == 0

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f p = f (fst p) (snd p)

eq :: Eq a => (a,a) -> Bool
eq = uncurry (==)

lessEq :: Ord a => (a,a) -> Bool
lessEq = uncurry (<=)

inverse :: ((a,b) -> c) -> ((b,a) -> c)
inverse f (x,y) = f (y,x)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

type Rel' a = a -> a -> Bool

emptyR' :: Rel' a
emptyR' = \ _ _ -> False

list2rel' :: Eq a => [(a,a)] -> Rel' a
list2rel' xys = \ x y -> elem (x,y) xys

idR' :: Eq a => [a] -> Rel' a
idR' xs = \ x y -> x == y && elem x xs

invR' :: Rel' a -> Rel' a
invR' = flip

inR' :: Rel' a -> (a,a) -> Bool
inR' = uncurry

reflR' :: [a] -> Rel' a -> Bool
reflR' xs r = and [ r x x | x <- xs ]

irreflR' :: [a] -> Rel' a -> Bool
irreflR' xs r = and [ not (r x x) | x <- xs ]

symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ not (r x y && not (r y x)) | x <- xs, y <- xs ]

transR' :: [a] -> Rel' a -> Bool
transR' xs r = and [ not (r x y && r y z && not (r x z)) | x <- xs, y <- xs, z <- xs ]

unionR' :: Rel' a -> Rel' a -> Rel' a
unionR' r s x y = r x y || s x y

intersR' :: Rel' a -> Rel' a -> Rel' a
intersR' r s x y = r x y && s x y

reflClosure' :: Eq a => Rel' a -> Rel' a
reflClosure' r = unionR' r (==)

symClosure' :: Rel' a -> Rel' a
symClosure' r = unionR' r (invR' r)

compR' :: [a] -> Rel' a -> Rel' a -> Rel' a
compR' xs r s x y = or [ r x z && s z y | z <- xs ]

repeatR' :: [a] -> Rel' a -> Int -> Rel' a
repeatR' xs r n 
        | n < 1 = error "argument < 1"
        | n == 1 = r
        | otherwise = compR' xs r (repeatR' xs r (n-1))

-- transClosure' :: [a] -> Rel' a -> Rel' a
-- maybe later..


{-- Chapter 5.5 --}

equivalenceR :: Ord a => Set a -> Rel a -> Bool
equivalenceR set r = reflR set r && symR r && transR r

equivalenceR' :: [a] -> Rel' a -> Bool
equivalenceR' xs r = reflR' xs r && symR' xs r && transR' xs r

modulo :: Integer -> Integer -> Integer -> Bool
modulo n = \ x y -> divides n (x-y)

equalSize :: [a] -> [b] -> Bool
equalSize list1 list2 = (length list1) == (length list2)

--------------------------------------------------------------------------

-- Self notes:
-- Pre-order (quasi-order):     transitive && reflexive
-- Strict partial order:        transitive && irreflexive
-- Partial order:               transitive && reflexive && antisymmetric
-- Total order:                 transitive && reflexive && antisymmetric && linear
--
-- If O is a set of properties of a relation on a set A, then the O-closure of a relation R is tehe smallest relation S that includes R and that has all the properties in O.
-- Most important ones:
--      Reflexive closure
--      Symmetric closure
--      Transitive closure
--      Reflexive transitive closure

--------------------------------------------------------------------------

-- Exercise 5.13
-- Answer:      For every x there is a link to something else from x
--              For every y there is a link of something else to y

-- Exercise 5.17
-- Answer:      The union or R on A is empty, so there are no 'self-connections'

-- Exercise 5.19
-- Answer:      1: The relation is symmetric if all x and y in R when xRy and yRx are equal
--              2: A relation is symmetric is the oppositive of the relation is the same, e.g. you can 'switch/flip' the connection.

-- Exercise 5.20
-- Answer       An irreflexive connection does not have 'self-connections', so do asymmetric relations

-- Exercise 5.22
-- Answer:      asymmetric:     xRy -> !yRx
--              antisymmetric:  (xRy ^ yRx) -> x=y OR x/=y -> xRy -> !yRx
--              In te equivalent formula, you can see that an asymm is always antisymm.

-- Exercise 5.28
-- Answer:      it is asymmatric because of the irreflexive connection (no self connections) and the transitive diabling xRy ^ yRx

-- Exercise 5.29
-- Answer:      irreflexive follows from asymmetric

-- Exercise 5.30
-- Answer:      ...

-- Exercise 5.31
-- Answer:      The reverse of the partial order is still equal to the orgional characteristics

-- Exercise 5.33
-- Answer:      Already discussed in the workshop.

-- Exercise 5.34
-- Answer:      Dont know how to do this.

-- Exercise 5.38
-- Answer:      The brother of my father is different than the father of my brother.

-- Exercise 5.39
-- Answer:      Already answered this in the workshop.

-- Exercise 5.40
-- Answer:      Every relation xRy will also be ySz