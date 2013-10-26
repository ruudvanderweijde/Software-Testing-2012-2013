import qualified Data.Tree as T
import Data.Tree.Pretty


data Tree k d = Nil | Node (Tree k d) k d (Tree k d)

example1, example2 :: Tree Int String
example1 = Node (Node Nil 4 "Hello" Nil) 7 "there" (Node Nil 21 "hi" Nil)
example2 = Node example1 34 "well" (Node Nil 55 "This" (Node (Node Nil 73 "one's" Nil) 102 "much" (Node Nil 132 "bigger" Nil)))

--data Tree a = Nil | Node a {
--        rootLabel :: a,         -- ^ label value
--        subForest :: T.Forest a   -- ^ zero or more child trees
--    }

toTree :: (Show k,Show d) => Tree k d -> T.Tree String
toTree Nil = T.Node "-" []
toTree (Node t1 k d t2) = T.Node (show k ++ ":" ++ show d) [toTree t1,toTree t2]

showtree :: (Show k, Show d) => Tree k d -> String
showtree = drawVerticalTree.toTree

printtree :: (Show k, Show d) => Tree k d -> IO ()
printtree = putStrLn.('\n':).showtree