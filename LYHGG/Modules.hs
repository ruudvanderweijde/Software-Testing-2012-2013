module Modules

where 

import Data.List
-- load function of module:
-- 	import Data.List (nub, sort)
-- load all functions besides:
-- 	import Data.List hiding (nub)
-- by importing qualified, you need to use the full name, 
-- this to avoid overwriting duplicate fucntion names
-- 	import qualified Data.Map as M (usage: M.filter)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
-- The Data.Set module offers us, well, sets. 
-- Like sets from mathematics. 
-- Sets are kind of like a cross between lists and maps. 
-- All the elements in a set are unique. 
-- And because they're internally implemented with trees 
-- (much like maps in Data.Map), they're ordered. 

--
-- Data.List functions:
--
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- nub leaves out duplicate items in a list.
--  "length . nub"  equivalents is "\xs -> length (nub xs)".

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)  

--
-- Data.Char functions:
--
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted  

decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg  

--
-- Data.Map functions:
--
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey' key [] = Nothing  
findKey' key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey' key xs  
findKey'' :: (Eq k) => k -> [(k,v)] -> v  
findKey'' key xs = snd . head . filter (\(k,v) -> key == k) $ xs  

-- We can implement our own fromList by using the empty map, insert and a fold. Watch:
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty  

--
-- Data.Set functions:
--

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  
