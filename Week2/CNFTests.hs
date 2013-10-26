module CNFTests

where

import Week2
import CNF
import Propositional
import PropositionalTests

-- precondition: a valid form
-- postcondition: returns true if "valid form" is equivalent with CNF("valid form")
testCNF :: Form -> Bool
testCNF f = equiv f g
		    where g = cnf f


t1 = testCNF (Dsj [Dsj [p,q],q])

{-
Coding the first version took +- 3 hours, it did not work at all 
missing was the Neg operator from the CNF algorithm. Took me 1 hour to
understand the Haskell not matching error, and the missing case.
-}

{- Dist check from lecture slides
First case uses equivalence of (p ^ q) _ r and (p _ r) ^ (q _ r).
Second case uses equivalence of p _ (q ^ r) and (p _ q) ^ (p _ r).
-}

dc1 = dist' p (Cnj [q, r]) == Cnj [Dsj [p, q], Dsj [p, r]]
dc2 = dist' (Cnj [q, r]) p == Cnj [Dsj [q, p], Dsj [r, p]]

-- Manually testing subfunctions.
--
-- *Week2> arrowfree form1
-- +(*(+(-1 2) +(--2 -1)) *(-+(-1 2) -+(--2 -1)))
-- *Week2> equiv (form1) (arrowfree form1)
-- True
-- *Week2> nnf . arrowfree $ form1
-- +(*(+(-1 2) +(2 -1)) *(*(1 -2) *(-2 1)))
-- *Week2> equiv form1 (nnf . arrowfree $ form1)
-- True
-- *Week2> (cnf . nnf . arrowfree $ form1)
-- *(*(*(+(+(-1 2) 1) +(+(-1 2) -2)) *(+(+(-1 2) -2) +(+(-1 2) 1))) *(*(+(+(2 -1) 1) +(+(2 -1) -2)) *(+(+(2 -1) -2) +(+(2 -1) 1))))
-- *Week2> equiv form1 (cnf . nnf . arrowfree $ form1)
-- True

{-
Testing

run test set composed of satisfiables, contradictions, and tautologies and test if there CNF is equivalent
with the original function.

-}

-- Run tests on predefined (and used) forms
testCNF1 = all testCNF [form1, form2, form3]
testCNF2 = all testCNF (allTaut ++ allCont ++ allSatis)

allCNFTests = all (\x -> x) [t1, dc1, dc2, testCNF1, testCNF2]
