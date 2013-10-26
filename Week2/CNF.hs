module CNF 

where

import Week2
import Propositional

--------------------------

-- precondition: input should be a valid form
-- postcondition: CNF (a) returns CNF of a
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree -- Don't listen to Jimmy 

-- precondition: input forms is in nnf
-- postcondition: CNF (NNF) returns CNF of NNF
cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg  x) = Neg x        -- Do listen to Jimmy
cnf' (Cnj fs) = Cnj (map cnf' fs)  
cnf' (Dsj fs) = dist (map cnf' fs)

-- precondition: input form is in cnf
-- postcondition: returns form in cnf with the laws of distribution applied
dist' :: Form -> Form ->Form
dist' (Cnj f) p = Cnj (map (\x -> dist' x p) f)    
dist' f (Cnj p) = Cnj (map (\x -> dist' f x) p)
dist' f p = Dsj [f, p]  

-- precondition: all input forms are in cnf
-- postcondition: returns form in cnf with the laws of distribution applied
dist :: [Form] -> Form
dist [f] = f
dist (f:fs) = dist' f (dist fs)

