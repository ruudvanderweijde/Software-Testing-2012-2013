module LabExam

where
import Data.List
import Assert

f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

data BinTree a = Nil | B a (BinTree a) (BinTree a) deriving (Eq,Show)

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value  (_,y) = y 

