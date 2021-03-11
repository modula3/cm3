GENERIC INTERFACE ArrayRef(Elem);
TYPE
  T = REF ARRAY OF Elem.T;
CONST
  New = Cat;

PROCEDURE Cat(READONLY a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,
              q,r,s,t,u,v,w,x,y,z := ARRAY OF Elem.T{}): T;

(* stops on first empty array *)

END ArrayRef.
