INTERFACE SmallBuffRd

(* A Rd class for testing that:
  1) Makes src(rd)[c] = VAL(c,CHAR), for easy checking
  2) Has very small buffers, to ease buffer-boundary-crossing
     cases.
*)  

; IMPORT Rd 

; TYPE T <: Rd . T 

; PROCEDURE New ( ) : T 

; END SmallBuffRd
. 
 
