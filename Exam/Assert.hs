module Assert

where 

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

forall = flip all

assert1 :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert1 p f x = if p x (f x) then f x 
                else error "assert1"

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"

assert3 ::  (a -> b -> c -> d -> Bool) 
             -> (a -> b -> c -> d) -> a -> b -> c -> d
assert3 p f x y z = 
  if p x y z (f x y z) then f x y z
  else error "assert3"

assert4 ::  (a -> b -> c -> d -> e -> Bool) 
             -> (a -> b -> c -> d -> e) 
             -> a -> b -> c -> d -> e
assert4 p f x y z u = 
  if p x y z u (f x y z u) then f x y z u
  else error "assert4"

assert5 ::  (a -> b -> c -> d -> e -> f -> Bool) 
             -> (a -> b -> c -> d -> e -> f) 
             -> a -> b -> c -> d -> e -> f
assert5 p f x y z u v = 
  if p x y z u v (f x y z u v) then f x y z u v
  else error "assert5"

invar1 :: (a -> Bool) -> (a -> a) -> a -> a
invar1 p f x = 
  let 
    x' = f x 
  in
  if p x && not (p x') then error "invar1"
  else x'

invar2 :: (a -> b -> Bool) -> 
          (a -> b -> (a,b)) -> 
           a -> b -> (a,b)
invar2 p f x y = 
  let 
    (x',y') = f x y 
  in 
    if p x y && not (p x' y') then error "invar2"
    else (x',y')

invar3 :: (a -> b -> c -> Bool) -> 
          (a -> b -> c -> (a,b,c)) -> 
           a -> b -> c -> (a,b,c)
invar3 p f x y z = 
  let 
    (x',y',z') = f x y z
  in 
   if p x y z && not (p x' y' z') then error "invar3"
   else (x',y',z')

invar4 :: (a -> b -> c -> d -> Bool) -> 
          (a -> b -> c -> d -> (a,b,c,d)) -> 
           a -> b -> c -> d -> (a,b,c,d)
invar4 p f x y z u = 
  let 
    (x',y',z',u') = f x y z u
  in 
   if p x y z u && not (p x' y' z' u')
   then error "invar4"
   else (x',y',z',u')

invar5 :: (a -> b -> c -> d -> e -> Bool) -> 
          (a -> b -> c -> d -> e -> (a,b,c,d,e)) -> 
           a -> b -> c -> d -> e -> (a,b,c,d,e)
invar5 p f x y z u v = 
  let 
    (x',y',z',u',v') = f x y z u v
  in 
   if p x y z u v && not (p x' y' z' u' v')
   then error "invar5"
   else (x',y',z',u',v')

assrt1 :: (Show a, Show b) => String 
                           -> (a -> b -> Bool)
                           -> (a -> b) -> a -> b 
assrt1 info p f x = 
  if p x (f x) then f x 
  else error ("assrt1:" ++ info ++ show(x,f x))

assrt2 :: (Show a, Show b,Show c) 
   => String  
   -> (a -> b -> c -> Bool) 
   -> (a -> b -> c) -> a -> b -> c
assrt2 info p f x y = 
  if p x y (f x y) then f x y
  else error ("assrt2:" ++ info ++ show(x,y,f x y))

assrt3 :: (Show a, Show b,Show c,Show d) 
  => String 
  -> (a -> b -> c -> d -> Bool) 
  -> (a -> b -> c -> d) -> a -> b -> c -> d
assrt3 info p f x y z = 
  if p x y z (f x y z) then f x y z
  else error ("assrt3" ++ info ++ show(x,y,z,f x y z))

assrt4 ::  (Show a, Show b,Show c,Show d,Show e) 
  => String 
  -> (a -> b -> c -> d -> e -> Bool) 
  -> (a -> b -> c -> d -> e) 
  -> a -> b -> c -> d -> e
assrt4 info p f x y z u = 
  if p x y z u (f x y z u) then f x y z u
  else error ("assrt4" ++ info ++ show(x,y,z,u,f x y z u))

assrt5 ::  (Show a, Show b,Show c,Show d,Show e,Show f) 
  => String 
  -> (a -> b -> c -> d -> e -> f -> Bool) 
  -> (a -> b -> c -> d -> e -> f) 
  -> a -> b -> c -> d -> e -> f
assrt5 info p f x y z u v = 
  if p x y z u v (f x y z u v) then f x y z u v
  else error ("assert5"++info++show(x,y,z,u,v,f x y z u v))

invr1 ::  Show a => String -> (a -> Bool) 
   -> (a -> a) ->  a -> a
invr1 info p f x = 
   if p x  && not (p (f x)) then 
      error ("invr1:" ++ info ++ show(x,f x))
   else f x

invr2 ::  (Show a, Show b) 
   => String  
   -> (a -> b -> Bool) 
   -> (a -> b -> (a,b))
   ->  a -> b -> (a,b)
invr2 info p f x y = 
  let 
    (x',y') = f x y 
  in 
   if p x y  && not (p x' y') then 
      error ("invr2:" ++ info ++ show(x,y,f x y))
   else (x',y')

invr3 ::  (Show a, Show b,Show c) 
   => String  
   -> (a -> b -> c -> Bool) 
   -> (a -> b -> c -> (a,b,c))  
   ->  a -> b -> c -> (a,b,c)
invr3 info p f x y z = 
  let 
    (x',y',z') = f x y z
  in 
   if p x y z && not (p x' y' z') then 
      error ("invr3:" ++ info ++ show(x,y,z,f x y z))
   else (x',y',z')

invr4 ::  (Show a, Show b,Show c,Show d) 
   => String  
   -> (a -> b -> c -> d -> Bool) 
   -> (a -> b -> c -> d -> (a,b,c,d))  
   ->  a -> b -> c -> d -> (a,b,c,d)
invr4 info p f x y z u = 
  let 
    (x',y',z',u') = f x y z u
  in 
   if p x y z u && not (p x' y' z' u') then 
      error ("invr4:" ++ info ++ show(x,y,z,u,f x y z u))
   else (x',y',z',u')

invr5 ::  (Show a, Show b,Show c,Show d,Show e) 
   => String  
   -> (a -> b -> c -> d -> e -> Bool) 
   -> (a -> b -> c -> d -> e -> (a,b,c,d,e))  
   ->  a -> b -> c -> d -> e -> (a,b,c,d,e)
invr5 info p f x y z u v = 
  let 
    (x',y',z',u',v') = f x y z u v
  in 
   if p x y z u v && not (p x' y' z' u' v') then 
      error ("invr5:" ++ info 
              ++ show(x,y,z,u,v,f x y z u v))
   else (x',y',z',u',v')

