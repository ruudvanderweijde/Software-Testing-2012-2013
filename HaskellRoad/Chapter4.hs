module Chapter4

where

import DB
import Query

run :: Integer -> [Integer]
run n   | n < 1     = error "argument is not positive"
        | n == 1    = [1]
        | even n    = n: run (div n 2)
        | odd n     = n: run (3*n+1)

--funny x | halts x x = undefined
--        | otherwise = True


q1 = [ x | x <- actors, directorP x ]
-- returned no results... added Tom Berenger as actor
q2 = [ (x,y) | (x,y) <- act, directorP x]
q3 = [ (x,y,z) | (x,y) <- direct, (u,z) <- release, y == u ]
q4 = q3
q5 = [ (x,y) | (x,y) <- direct, (u,"1995") <- release, y == u ]
q6 = [ (x,y,z) | (x,y) <- direct, (u,z) <- release, y == u, z > "1995" ]
q7 = [ x | ("Kevin Spacey",x) <- act ]
q8 = [ x | (x,y) <- release, y > "1997", actP ("William Hurt", x) ]
q9 = q1 /= []
q10 = [ x | ("Woody Allen",x) <- direct ] /= [] 
q10' = directorP "Woody Allen"

q11 = [ y | (x,y) <- act , x == "Robert De Niro" || x == "Kevin Spacey" ]
-- two functions to check for correctness...
q11' = [ x | ("Robert De Niro", x) <- act ] 
q11'' = [ x | ("Kevin Spacey", x) <- act ] 
