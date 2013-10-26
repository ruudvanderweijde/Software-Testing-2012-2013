module FOL

where

import Data.List
import Data.Char
import System.Random

import Week3
import Techniques
import Permutation
import CNFTests
import TestCNF

getRandomTerm :: IO Term
getRandomTerm = do
        n <- getRandomInt 2
        case n of
                0 -> do
                        return x
                1 -> do
                        return y
                2 -> do
                        return z
--x = V "x"
--y = V "y"
--z = V "z"


getRandomName :: IO Char
getRandomName = randomRIO ('A','Z')

getRandomFormula :: IO Formula
getRandomFormula = do 
        d <- getRandomInt 10 
        getRandomFormula' d
            
getRandomFormula' :: Int -> IO Formula 
getRandomFormula' 0 = do 
        name <- getRandomName
        return (Atom [name] [])

getRandomFormula' d = do 
        n <- getRandomInt 8
        case n of 
                0 -> do 
                        name <- getRandomName
                        term <- getRandomTerm
                        return (Atom [name] [term])
                1 -> do
                        f1 <- getRandomTerm
                        f2 <- getRandomTerm
                        return (Eq f1 f2)
                2 -> do
                        f <- getRandomFormula' (d-1)
                        return (Neg f)
                3 -> do
                        f1 <- getRandomFormula' (d)
                        f2 <- getRandomFormula' (d-1)
                        return (Impl f1 f2)
                4 -> do
                        f1 <- getRandomFormula' (d)
                        f2 <- getRandomFormula' (d-1)
                        return (Equi f1 f2)
                5 -> do
                        m <- getRandomInt 5
                        fs <- getRandomFormulas' (d-1) m
                        return (Conj fs)
                6 -> do
                        m <- getRandomInt 5
                        fs <- getRandomFormulas' (d-1) m
                        return (Disj fs)
                7 -> do
                        name <- getRandomName
                        f <- getRandomFormula' (d-1)
                        return (Forall [name] f)
                8 -> do
                        name <- getRandomName
                        f <- getRandomFormula' (d-1)
                        return (Exists [name] f)

getRandomFormulas :: Int ->  IO [Formula]
getRandomFormulas n = do 
        d <- getRandomInt 3
        getRandomFormulas' d n     

getRandomFormulas' :: Int -> Int -> IO [Formula]
getRandomFormulas' _ 0 = return []
getRandomFormulas' d n = do 
        f <- getRandomFormula' d
        fs <- getRandomFormulas' d (n-1) 
        return (f:fs)

-- `Test` results:
--        *Assigments> getRandomFormula
--        (conj[E D (disj[~disj[E F conj[I,Y,D,P,Y],E Z A P P,(false<=>y==y),(A S conj[U,R,U,J,M]<=>(E R I==>L)),A Y E[z]],E[x],E Y ~~(disj[G]==>E),E N (N[z]<=>conj[D[z],x==z]),x==z]==>(E I J[x]<=>M[y])),K[y],~false,conj[(false<=>(E L x==z<=>disj[~~Q,~(H[y]<=>A),~~N,z==x])),false,conj[(~Q[x]==>~E H A N Q),A Y x==y,(E C A J x==y==>disj[z==x,z==x,~disj[X]])],E C y==x,disj[(E A E H I[y]==>disj[~X[x],F[z]]),conj[R[y],conj[L[z],~~W],~A J U[z],((E R false<=>y==y)<=>conj[(E B H==>M),(conj[L,W,H,L,X]==>Z),((~B<=>R)<=>S),A A V,A Q K]),(conj[x==y,A O E C J,~A Q D,((A G A B A<=>(E C U<=>Y))==>x==y)]<=>E J disj[F,Z,E,Z,Q])],x==y,P[z],z==x]]]<=>x==z)
--        *Assigments> getRandomFormula
--        true
--        *Assigments> getRandomFormula
--        x==x
--        *Assigments> getRandomFormula
--        A F z==y
--        *Assigments> getRandomFormula
--        E M disj[false,E S y==x,((y==x<=>~E K A[z])<=>A W ((conj[z==y,N[x],E I (disj[E P (disj[true,E N Q]==>A N Q),y==y,(conj[(A Y E H V==>disj[U,S,H,R])]<=>(disj[E U X,conj[O,P,T,J]]<=>(((~U==>N)<=>W)<=>I)))]==>((false==>y==y)==>E W ~Y)),A O (z==x==>conj[((disj[(disj[U,W,N,Z]<=>D),(z==x<=>Z),(x==y==>S),disj[J],E D M]<=>F[z])==>Z[x]),disj[disj[G,W,M,W,J]],A H R[y],X[z]]),disj[~(E L (((x==y<=>conj[G,I,I,X])<=>~P)==>z==x)==>true),A Z (Y[z]==>A W A W F),E Y ~(E A E W S<=>E U T),V[y]]]==>(y==z==>(conj[z==y,~(((J[y]<=>x==y)==>E B Q)<=>(z==z<=>H)),((disj[E K (~G==>T),E E C[z],conj[disj[Z,H],(~T==>U),disj[U]],conj[(conj[N]<=>R),(A E S<=>C),(disj[W,M,B]==>E)]]==>y==z)==>conj[E O V,conj[M,Y,T,X],false,x==x,y==z]),~(conj[A R C,false,((A[x]==>Q)==>P),(conj[J]<=>R),A Y K]==>A B Y),z==y]<=>~A F conj[N])))==>E F W[y])),(E O E J A Y E K disj[E T disj[E R F],~(false==>x==x),conj[~E G V],disj[x==y,D[y],((z==x==>A Q I)==>disj[Q,W,H])]]==>true)]
--
-- Time spent: about 1,5 hours
