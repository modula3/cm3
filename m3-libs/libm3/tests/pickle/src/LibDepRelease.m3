MODULE LibDepRelease EXPORTS LibDep 

(* Things that need different source code, depending on the version of 
   m3core and libm3.  Version for newer libraries that are are at least
   aware of 21-biut WIDECHAR, alhtough they could be compiled with
   16-bit WIDECHAR. *)  

; IMPORT Pickle2 AS Pickle 
; IMPORT TextClass  
; IMPORT Thread 
; IMPORT Wr 

; PROCEDURE PickleWrite 
     ( wr : Wr . T ; r : REFANY ; write16BitWidechar : BOOLEAN := FALSE )
  RAISES { Pickle . Error , Wr.Failure , Thread.Alerted }

  = BEGIN 
      Pickle . Write ( wr , r ) 
    END PickleWrite 
  
; PROCEDURE SetTextClassDotFlatten ( Val : BOOLEAN ) 

  = BEGIN 
    (* TextClass . Flatten := Val *) 
    (* I can't do that Dave.  TextClass in the release doesn't have Flatten. *) 
    END SetTextClassDotFlatten

; BEGIN 
  END LibDepRelease 
. 
