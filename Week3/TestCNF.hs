module TestCNF

where

import Week2
import CNF
import CNFTests
import Propositional
import PropositionalTests

import Data.List
import Data.Char
import System.Random

import Week3
import Techniques
import Permutation
import CNFTests

testCNF :: IO ()
testCNF = testForms 100
   (\ f -> let [g] = parse (show f) in testCNF' f == testCNF' g)

-- Tests of 10000 forms:
        --"pass on:+(+(20 4 3 4) -3)"
        --"pass on:+(-15 -15 9)"
        --"pass on:-16"
        --"pass on:10"
        --"pass on:8"
        --"pass on:16"
        --"pass on:+(+(11 13 14 3) -10 8 6 +())"
        --"pass on:--15"
        --"pass on:5"
        --"pass on:+(+(13 2 15) +(17 6) -14)"
        --"pass on:+()"
        --"pass on:*(5)"
        --"pass on:2"
        --"pass on:-17"
        --"pass on:+(8 21 *(17 15 18 18))"
        --"pass on:*(7 7 -1)"
        --"pass on:--13"
        --"pass on:+(+() 15 +(20) +(21) +(17 7 8 18))"
        --"pass on:*(-7 19 *() 16 +(7 10 10))"
        --"pass on:-12"
        --"pass on:+(7 -3)"
        --"pass on:+(+(15) -21)"
        --"pass on:-3"
        --"pass on:*(-20 *() *(7 14 15) *(17 14))"
        --"pass on:2"
        --"pass on:6"
        --"pass on:+()"
        --"pass on:+(18 *(15 6 2))"
        --"pass on:*(14 *() -8 -14 -17)"
        --"10000 tests passed"

-- also ran 100 forms many times, no errors.
--
-- Time spent about 2 hours.