module PropositionalTests

where

import Week2
import Propositional

-- form1 and form3 are commented because they are tautologies
form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

-- Tautologies [list of function found at: http://www.millersville.edu/~bikenaga/math-proof/truth-tables/truth-tables.html]
-- 1. The law of the excluded middle.
taut1 = Dsj [Neg p, p] 
-- 2. Contradiction.
taut2 = Neg (Cnj [p, Neg p])
-- 3. Modus tolens
taut3 = Impl (Cnj [Impl (p) (q),Neg q]) (Neg p)
-- 4. Double negation
taut4 = Equiv (Neg (Neg p)) (p)
-- 5. Law of syllogism
taut5 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
-- 6. Decomposing a conjuntion
taut6  = Impl (Cnj [p, q]) (p)
taut6' = Impl (Cnj [p, q]) (q)
-- 7. Constructing a disjunction
taut7  = Impl (p) (Dsj [p, q])
taut7' = Impl (q) (Dsj [p, q])
-- 8. Definition of the biconditional
taut8 = Equiv (Equiv p q) (Cnj [Impl p q, Impl q p])
-- 9. Communicative law for ^
taut9 = Equiv (Cnj [p, q]) (Cnj [q, p])
-- 10. Communicative law for V
taut10 = Equiv (Dsj [p, q]) (Dsj [q, p])
-- 11. Associative law for ^
taut11 = Equiv (Cnj [Cnj [p, q], r]) (Cnj [p, Cnj [q, r]])
-- 12. Associative law for V
taut12 = Equiv (Dsj [Dsj [p, q], r]) (Dsj [p, Dsj [q, r]])
-- 13. DeMorgan's law
taut13 = Equiv (Neg (Cnj [p, q])) (Dsj [(Neg p), (Neg q)])
-- 14. DeMorgan's law
taut14 = Equiv (Neg (Dsj [p, q])) (Cnj [(Neg p), (Neg q)])
-- 15. Distributivity
taut15 = Equiv (Cnj [p, Dsj [q, r]]) (Dsj [Cnj [p, q], Cnj [p, r]])
-- 16. Distributivity
taut16 = Equiv (Dsj [p, Cnj [q, r]]) (Cnj [Dsj [p, q], Dsj [p, r]])
-- 17. Contrapostive
taut17 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- 18. Conditional disjunction
taut18 = Equiv (Impl p q) (Dsj [Neg p, q])
-- 19. Disjuntive syllogism
taut19 = Impl (Cnj [Dsj [p, q], Neg p]) (q)
-- 20. Simplification
taut20 = Equiv (Dsj [p, p]) (p)
-- 21. Simplification
taut21 = Equiv (Cnj [p, p]) (p)
-- 22. 23. Two others
taut22 = Impl (Cnj [Impl (Neg p) q, Impl (Neg p) (Neg q)]) (p) -- reductio ad absurdum.
taut23 = Impl (Cnj [(Dsj [p, q]), (Impl p r), (Impl q r)]) (r)-- proof by cases.

-- Contradictions
cont1 = Cnj [p, Neg p] 
cont2 = Neg (Impl p p)

-- Satisfiable (ofcourse, all tautologies are also satisfiable)
satis1 = p
satis2 = Neg p
satis3 = Impl p q

-- Source: http://homepages.cwi.nl/~jve/books/pdfs/lai.pdf (p 98.)
entails1a = Impl p (Impl q r)
entails1b = Impl (Impl p q) (Impl p r)
entails2a = Cnj [p, Impl p q]
entails2b = q
entails3a = Cnj [Impl p q, Neg q]
entails3b = Neg p
entails4a = Cnj [Impl p q, Impl q r]
entails4b = Impl p r
entails5a = Cnj [Dsj [p, q], Impl p r]
entails5b = Dsj [r, q]

-- Source: http://homepages.cwi.nl/~jve/books/pdfs/lai.pdf (p 99.)
equiv1a = p
equiv1b = Neg $ Neg p
equiv2a = Neg p
equiv2b = Impl p (Cnj [q, Neg q])
equiv3a = Neg (Cnj [p, q])
equiv3b = Dsj [Neg p, Neg q]
equiv4a = Neg (Dsj [p, q])
equiv4b = Cnj [Neg p, Neg q]

-- Joined variables
allTaut = [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23]
allCont = [cont1, cont2]
allSatis = [satis1, satis2, satis3]

-- Tests:
testT1 = all tautology allTaut
testT2 = all satisfiable [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23] 
testT3 = not (any contradiction [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23]) 

testC1 = all contradiction allCont
testC2 = not (any satisfiable allCont)

testS1 = all satisfiable allSatis
testS2 = not (any contradiction allSatis)
testS3 = not (any tautology allSatis)


-- Tests for entails
testE1 = entails entails1a entails1b
testE2 = entails entails2a entails2b
testE3 = entails entails3a entails3b
testE4 = entails entails4a entails4b
testE5 = entails entails5a entails5b

-- Tests for equiv
testEq1 = equiv equiv1a equiv1b
testEq2 = equiv equiv2a equiv2b
testEq3 = equiv equiv3a equiv3b
testEq4 = equiv equiv4a equiv4b

-- A test for all tests
allPropositionalTests = all (\x -> x) 
	[testT1, testT2, testT3, 
	testC1, testC2, 
	testS1, testS2, testS3,
	testE1, testE2, testE3, testE4, testE5,
	testEq1, testEq2, testEq3, testEq4]
