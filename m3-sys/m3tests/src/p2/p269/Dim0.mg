(* ----------------------------------------------------------------------1- *)
(* File Dim0.mg for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Dim0 ( Elts , Shape )

(* EXPORTED*) 
; PROCEDURE EltVal
    ( <*UNUSED*> READONLY A : OpenTyp ; <*UNUSED*> Subs : DimSubsTyp ) : EltTyp
  
    = BEGIN
        <* ASSERT FALSE *>
      END EltVal

(* EXPORTED*) 
; PROCEDURE SetEltVal
    ( <*UNUSED*> VAR A : OpenTyp
    ; <*UNUSED*> Subs : DimSubsTyp
    ; <*UNUSED*> EltVal : EltTyp
    ) 
  
    = BEGIN
        <* ASSERT FALSE *>
      END SetEltVal

; BEGIN
  END Dim0
.


      
