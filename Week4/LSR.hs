module LSR 

where

import Data.List

evens1 = [ n | n <- [0..], even n ] 

evens2 = [ 2*n | n <- [0..] ]

naturals = [0..] 
small_squares1 = [ n^2 | n <- [0..999] ]
small_squares2 = [ n^2 | n <- naturals , n < 1000 ]
small_squares3 = take 1000 [ n^2 | n <- naturals ]

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))
                     