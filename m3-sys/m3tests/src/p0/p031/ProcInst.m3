(* ----------------------------------------------------------------------1- *)
(* File ProcInst.m3 for Modula3 compiler test p031                          *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE ProcInst

; EXCEPTION Ex 

; PROCEDURE Val1 ( ) RAISES ANY
  = VAR B : BOOLEAN
  ; BEGIN
      B := FALSE
    ; IF B THEN RAISE Ex END
      (* All this, just to squelch compile-time warning. *) 
    END Val1 

; PROCEDURE Val2 ( ) RAISES ANY
  = VAR B : BOOLEAN
  ; BEGIN 
      B := FALSE
    ; IF B THEN RAISE Ex END
      (* All this, just to squelch compile-time warning. *) 
    END Val2 

; BEGIN
  END ProcInst
.

