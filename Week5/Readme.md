Assignments are placed in Assignment.hs
========


    Exercise 1 

    Assertion: mergeSrtA 
        - checks if the output is a permutation of the input
        - checks if the sorted input is the same as the output
    Indication of time spent: about 30 minutes.


 
    Exercise 2 

    Assertion: mergeSrtSplitA 
        - checks if the output is a permutation of the input
        - checks if the sorted input is the same as the output
        - checks if all splitted lists are sublists
    Indication of time spent: about 30 minutes.


 
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



    Exercise 4

    For the random sudoku generator to be able to work properly I only had to replace the Week5 module for the Week5modified module.

    Indication of time spent: 30 minutes.



    Exercise 5

    How to find out that the generated sudoku is minimal?
        - Remove all the items one by one, and see if the sudoku becomes ambigious.

    Indication of time spent: 2 hours.

