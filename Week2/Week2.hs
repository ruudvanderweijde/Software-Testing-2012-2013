module Week2

where

import Data.List

type Name = Int

data Form
	= Prop Name
	| Neg Form
	| Cnj [Form]
	| Dsj [Form]
	| Impl Form Form
	| Equiv Form Form
	deriving Eq

p = Prop 1
q = Prop 2
r = Prop 3

type Valuation = [(Name, Bool)]

-- all possible valuations for list of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) =
	map 	((name, True) 	:) (genVals names)
	++ map 	((name, False) 	:) (genVals names)

-- generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

instance Show Form where
	show (Prop x)		= show x
	show (Neg f)		= '-' : show f
	show (Cnj fs)		= "*(" ++ showLst fs ++ ")"
	show (Dsj fs)		= "+(" ++ showLst fs ++ ")"
	show (Impl f1 f2)	= "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
	show (Equiv f1 f2)	= "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ' : show f ++ showRest fs

propNames :: Form -> [Name]
propNames = sort.nub.pnames where
	pnames (Prop name)		= [name]
	pnames (Neg f)			= pnames f
	pnames (Cnj fs)			= concat (map pnames fs)
	pnames (Dsj fs)			= concat (map pnames fs)
	pnames (Impl f1 f2)		= concat (map pnames [f1, f2])
	pnames (Equiv f1 f2)	= concat (map pnames [f1, f2])

eval :: Valuation -> Form -> Bool
eval [] (Prop c)	= error ("no info: " ++ show c)
eval ((i,b):xs) (Prop c)
	| c == i 	= b
	| otherwise	= eval xs (Prop c)
eval xs (Neg f)			= not (eval xs f)
eval xs (Cnj fs)		= all (eval xs) fs
eval xs (Dsj fs)		= any (eval xs) fs
eval xs (Impl f1 f2)	= not (eval xs f1) || eval xs f2
eval xs (Equiv f1 f2)	= eval xs f1 == eval xs f2

-- no precondition: should work for any formula. 
arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2
-- postcondition: output is arrow-free

-- precondition: input is arrow-free
nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
-- postcondition: NFF (f) returns NNF of f
