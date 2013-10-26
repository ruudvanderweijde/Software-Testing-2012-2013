module Assignment

where

import Data.List
import Week4 
--import Week5
import Week5modified
import NRCSudoku
import RandomSudoku

{- 
    Exercise 1 

    Assertion: mergeSrtA 
        - checks if the output is a permutation of the input
        - checks if the sorted input is the same as the output
    Indication of time spent: about 30 minutes.
-}

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- The output is a permutation of the input
permutationProp :: Ord a => [a] -> [a] -> Bool
permutationProp xs ys = isPermutation xs ys

-- The output list is the sorted version of the input 
sortedOutputProp :: Ord a => [a] -> [a] -> Bool
sortedOutputProp xs ys = sort xs == ys

-- Length of input and output of the list is the same
-- no longer used, used permutation instead because it implicates length
lengthProp :: Ord a => [a] -> [a] -> Bool
lengthProp xs ys = length xs == length ys

-- isPermutation compares two lists and returns True if the list is a permutation
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []             = True
isPermutation [] _              = False
isPermutation _ []              = False
isPermutation (x:xs) (y:ys)  
        | x == y = isPermutation xs ys
        | otherwise = isPermutation xs (delete x (y:ys))

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 permutationProp 
          $ assert1 sortedOutputProp 
            mergeSrt


{- 
    Exercise 2 

    Assertion: mergeSrtSplitA 
        - checks if the output is a permutation of the input
        - checks if the sorted input is the same as the output
        - checks if all splitted lists are sublists
    Indication of time spent: about 30 minutes.
-}

mergeSrtSplit :: Ord a => [a] -> [a]
mergeSrtSplit []  = []
mergeSrtSplit [x] = [x]
mergeSrtSplit xs  = merge
                        (assert1 sublistProp1 mergeSrtSplit (fst ys)) 
                        (assert1 sublistProp1 mergeSrtSplit (snd ys))
        where ys = split xs

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
           in (take n xs, drop n xs)

sublistProp1 :: Eq a => [a] -> [a] -> Bool
sublistProp1 xs ys = sublist xs ys

mergeSrtSplitA :: Ord a => [a] -> [a]
mergeSrtSplitA = assert1 permutationProp 
               $ assert1 sortedOutputProp 
               $ assert1 sublistProp1
                 mergeSrtSplit

{- 
    Exercise 3 

    Changes in specifications:
        - A NRC Sudoku has the same constraints as the 'normal' sudoku.
        - There is one addition to the rules:
        – Every innersubgrid [i; j] with i; j ranging over 2..4 and 6..8 should contain each number in {1, ..., 9}

    Modifications in Week5modified (full details in the file itself):
        - update of print/show function to add the extra grids in the interface
        - besides blocks, there are also subblocks
                innerblocks :: [[Int]]
                innerblocks = [[2..4],[6..8]]
        - check col or row for innerblock position
                innerbl :: Int -> [Int]         
                innerbl x = concat $ filter (elem x) innerblocks 
        - define innersubGrid
                innersubGrid :: Sudoku -> (Row,Column) -> [Value]               
                innersubGrid s (r,c) =          
                    [ s (r',c') | r' <- innerbl r, c' <- innerbl c ]
        - get free values of a subgrid
                freeInInnerSubgrid :: Sudoku -> (Row,Column) -> [Value]         
                freeInInnerSubgrid s (r,c) = freeInSeq (innersubGrid s (r,c))
        - added constraint to freeAtPos
                `intersect` (freeInInnerSubgrid s (r,c))
        - function to see if an innersubgrid is injective
                innersubgridInjective :: Sudoku -> (Row,Column) -> Bool         
                innersubgridInjective s (r,c) = injective vs where              
                    vs = filter (/= 0) (innersubGrid s (r,c))
        - added constraint to consistent function
                ++ [ innersubgridInjective s (r,c) | r <- [2,6], c <- [2,6]]
        - added constraint to sameblock function
                || (innerbl r == innerbl x && innerbl c == innerbl y
    
    Testing:
        - I've tested 'normal' sudoku's and they are either invalid, or have multiple solutions.
        - Stripped a list of nrcSudoku's of a website (http://www.hoeheethet.com/sudoku/sudoku.php) and tested them on my modified function.

    Indication of time spent: 2 hours.
-}

{- 
    Exercise 4

    For the random sudoku generator to be able to work properly I only had to replace the Week5 module for the Week5modified module.

    Indication of time spent: 30 minutes.
-}

{-
    Exercise 5

    How to find out that the generated sudoku is minimal?
        - Remove all the items one by one, and see if the sudoku becomes ambigious.

        *Assignment> testSudoku
        "Running 7 tests."
        "Test successful."
        "Test successful."
        "Test successful."
        "Test successful."
        "Test successful."
        "Test successful."
        "Test successful."
        "All done"
        *Assignment> testSudoku
        "Running 4 tests."
        "Test successful."
        "Test successful."
        "Test successful."
        "Test successful."
        "All done"

    Indication of time spent: 2 hours.
-}

-- generate a problem
-- remove all the items one by one
-- if solution is not a singleton, problem is not minimal!

testSudoku :: IO ()
testSudoku = do
        r <- getRandomInt 9
        print ("Running " ++ show (r+1) ++ " tests.")
        testSudoku' (r+1)

testSudoku' :: (Eq a, Num a) => a -> IO ()
testSudoku' 0 = print("All done");
testSudoku' n = do 
        [r] <- rsolveNs [emptyN]
        node  <- genProblem r
        if (testSudokuMinimal node) then do
             print ("Test successful.")
             testSudoku' (n-1)
        else do 
             print ("Test FAILED for problem:")
             showNode node
        

testSudokuMinimal :: Node -> Bool
testSudokuMinimal node = testSudokuRemoveItems node xs
    where xs = filledPositions (fst node)

-- remove items one by one and check if the sudoku is still unique
testSudokuRemoveItems :: Node -> [(Row,Column)] -> Bool
testSudokuRemoveItems _ [] = True
testSudokuRemoveItems (s,c) (x:xs) 
        | uniqueSol (s', c') = False
        | otherwise          = testSudokuRemoveItems (s,c) xs
        where s' = eraseS s x
              c' = constraints s'

{- Example of tested sudoku problem: 
    +---------+---------+---------+
    | 3       |       9 |         |
    |   +-----|--+   +--|-----+   |
    |   |     | 1|   |  |   8 | 2 |
    |   |   4 |  |   |  |     |   |
    +---------+---------+---------+
    | 8 |   5 |  |   |  | 2   |   |
    |   +-----|--+   +--|-----+   |
    |         |    4    |         |
    |   +-----|--+   +--|-----+   |
    | 9 |     |  |   |  |     |   |
    +---------+---------+---------+
    |   |     |  |   |  | 9   |   |
    | 4 | 5 3 |  |   |  |     |   |
    |   +-----|--+   +--|-----+   |
    |         |         |   6   1 |
    +---------+---------+---------+
-}
unique1 :: Grid
unique1 = [[3,0,0,0,0,9,0,0,0],
           [0,0,0,1,0,0,0,8,2],
           [0,0,4,0,0,0,0,0,0],
           [8,0,5,0,0,0,2,0,0],
           [0,0,0,0,4,0,0,0,0],
           [9,0,0,0,0,0,0,0,0],
           [0,0,0,0,0,0,9,0,0],
           [4,5,3,0,0,0,0,0,0],
           [0,0,0,0,0,0,0,6,1]]

-- used to manually test, by randomly removing one of the items.
notUnique1 :: Grid
notUnique1 = [[0,0,0,0,0,9,0,0,0],
             [0,0,0,1,0,0,0,8,2],
             [0,0,4,0,0,0,0,0,0],
             [8,0,5,0,0,0,2,0,0],
             [0,0,0,0,4,0,0,0,0],
             [9,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,9,0,0],
             [4,5,3,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,6,1]]

{-
    Exercise extra:
        - Define the difficulty of a generated problem

    #1 Calc the number of search paths.
-}

measureSudoku = do 
        [r] <- rsolveNs [emptyN]
        node  <- genProblem r
        showNode node
        showConstraints node
