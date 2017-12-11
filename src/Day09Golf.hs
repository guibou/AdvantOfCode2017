{-# LANGUAGE NoTemplateHaskell #-}
import Prelude
-- 185
g s=snd$(1,(0,0))?s
r?[]=r
r@(z,(c,g))?(y:q)=case y of '{'->(z+1,(c+z,g))?q;'}'->(z-1,(c,g))?q;','->r?q;'<'->let(h,p)=0#q in(z,(c,g+h))?p
c#('>':x)=(c,x)
c#('!':_:x)=c#x
c#(_:x)=(c+1)#x
