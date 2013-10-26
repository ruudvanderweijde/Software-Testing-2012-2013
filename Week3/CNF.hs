module CNF 

where

import Week2
import Propositional

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f1:f2:fs)) = dist f1 f2
cnf f = f
-- postcondition: CNF (f) returns CNF of f

-- precondition: input are in CNF 
dist :: Form -> Form -> Form
dist (Cnj []) _                 = Cnj []
dist (Cnj [f1]) f2              = dist f1 f2
dist (Cnj (f1:fs)) f2   = Cnj [(dist f1 f2), (dist (Cnj fs) f2)]
dist _ (Cnj [])                 = Cnj []
dist f1 (Cnj [f2])              = dist f1 f2
dist f1 (Cnj (f2:fs))   = Cnj [(dist f1 f2), dist f1 (Cnj fs)]
dist f1 f2                              = Dsj [f1,f2]
-- postcondition: DIST(f1,f2) returns CNF of Dsj [f1, f2]
