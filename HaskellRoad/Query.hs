module Query 

where 

import DB
import Data.List

characters = nub [ x       | ["character",x] <- db ]
movies     =     [ x       | ["movie",x]     <- db ]
actors     = nub [ x       | ["actor",x]     <- db ]
directors  = nub [ x       | ["director",x]  <- db ]
dates      = nub [ x       | ["release",_,x] <- db ]
universe   = nub (characters++actors++directors++movies++dates)

direct     = [ (x,y)   | ["direct",x,y]  <- db ]
act        = [ (x,y)   | ["play",x,y,_]  <- db ]
play       = [ (x,y,z) | ["play",x,y,z]  <- db ]
release    = [ (x,y)   | ["release",x,y] <- db ]

charP      = \ x       -> elem x characters
actorP     = \ x       -> elem x actors
movieP     = \ x       -> elem x movies
directorP  = \ x       -> elem x directors
dateP      = \ x       -> elem x dates
actP       = \ (x,y)   -> elem (x,y) act
releaseP   = \ (x,y)   -> elem (x,y) release
directP    = \ (x,y)   -> elem (x,y) direct
playP      = \ (x,y,z) -> elem (x,y,z) play
