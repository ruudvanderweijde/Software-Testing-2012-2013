module Lab6

where
import Data.List
import Data.Char
import System.Random
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Workshop 6
data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

exampleTree :: Btree String
exampleTree = Node (Node (Leaf "Hoare, Tony")
        (Leaf "Turing, Alan"))
        (Leaf "Goedel, Kurt")

exampleTree' :: Btree String
exampleTree' = Node (Node (Leaf "Hoare, Tony") (Leaf "Turing, Alan"))
                    (Node (Leaf "Goedel, Kurt") (Leaf "Weijde, Ruud van der"))
              

depthB :: Btree a -> Int
depthB (Leaf _)     = 0
depthB (Node t1 t2) = max (depthB t1) (depthB t2) + 1

balanced :: Btree a -> Bool
balanced (Leaf _)     = True
balanced (Node t1 t2) = balanced t1 && balanced t2 && depthB t1 == depthB t2

leafCount :: Btree a -> Int 
leafCount (Leaf _)     = 1
leafCount (Node t1 t2) = leafCount t1 + leafCount t2

mapB :: (a -> b) -> Btree a -> Btree b
mapB f (Leaf x)     = Leaf (f x)
mapB f (Node t1 t2) = Node (mapB f (t1)) (mapB f (t2))

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1, example1, example1]

count :: Tree a -> Int
count (T _ []) = 1
count (T _ xs) = 1 + sum (map count xs)

count' :: Tree a -> Int
count' (T _ xs) = 1 + sum (map count' xs)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x ys) = T (f x) (map (mapT f) ys)


-- from slides of week 6
fermatTest n = [] == filter (/=1) [ a^(n-1) `mod` n | a <- [1..(n-1)]]
