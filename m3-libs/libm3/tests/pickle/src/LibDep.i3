INTERFACE LibDep

(* Things that need different source code, depending on the version of 
   m3core and libm3. *)  

; IMPORT Pickle2 AS Pickle  
; IMPORT Thread 
; IMPORT Wr 

; PROCEDURE PickleWrite 
     ( wr : Wr . T ; r : REFANY ; write16BitWidechar : BOOLEAN := FALSE )
   RAISES { Pickle . Error , Wr.Failure , Thread.Alerted }

; PROCEDURE SetTextClassDotFlatten ( Val : BOOLEAN ) 
  
; END LibDep 
 
. 
