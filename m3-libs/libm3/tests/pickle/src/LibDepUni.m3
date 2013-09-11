MODULE LibDepUni EXPORTS LibDep 

(* Things that need different source code, depending on the version of 
   m3core and libm3.  Version for older libraries that corresponded to
   when only 16-bit WIDECHAR existed. *)  

; IMPORT Pickle2 AS Pickle  
; IMPORT TextClass 
; IMPORT Thread 
; IMPORT Wr 

; PROCEDURE PickleWrite 
     ( wr : Wr . T ; r : REFANY ; write16BitWidechar : BOOLEAN := FALSE )
  RAISES { Pickle . Error , Wr.Failure , Thread.Alerted }

  = BEGIN 
      Pickle . Write ( wr , r , write16BitWidechar ) 
    END PickleWrite 
  
; PROCEDURE SetTextClassDotFlatten ( Val : BOOLEAN ) 

  = BEGIN 
      TextClass . Flatten := Val 
    END SetTextClassDotFlatten

; BEGIN 
  END LibDepUni 
. 
