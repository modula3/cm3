INTERFACE Display

; TYPE Rng = [ 0 .. 191 ] 

; TYPE ArrT = ARRAY [ 0 .. 2 ] OF INTEGER 
(* ----------------------------------------------------------------------1- *)
(* File Display.i3 for Modula3 compiler test p288                           *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

; TYPE RecT = RECORD Fld0 , Fld1 , Fld2 : INTEGER END 
; TYPE SetT = SET OF Rng
; TYPE OuterRecT = RECORD
    A : ArrT 
  ; R : RecT
  ; S : SetT
  END 

; PROCEDURE Put ( Text : TEXT )
; PROCEDURE PutLine ( Text : TEXT )

; PROCEDURE A ( READONLY DFormF : ArrT ) 
; PROCEDURE R ( READONLY DFormR : RecT ) 
; PROCEDURE S ( READONLY DFormS : SetT )

; PROCEDURE Rec ( VAR Outer : OuterRecT ) 

; END Display
.


