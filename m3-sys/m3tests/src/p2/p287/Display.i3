(* ----------------------------------------------------------------------1- *)
(* File Display.i3 for Modula3 compiler test p287                           *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Display

; TYPE Rng = [ 0 .. 191 ] 

; TYPE ArrFT = ARRAY [ 0 .. 2 ] OF INTEGER 
; TYPE ArrOT = ARRAY  OF INTEGER 
; TYPE RecT = RECORD Fld0 , Fld1 , Fld2 : INTEGER END 
; TYPE SetT = SET OF Rng

; PROCEDURE Put ( Text : TEXT )

; PROCEDURE F ( READONLY DFormF : ArrFT ) 
; PROCEDURE O ( READONLY DFormO : ArrOT ) 
; PROCEDURE R ( READONLY DFormR : RecT ) 
; PROCEDURE S ( READONLY DFormS : SetT ) 

; END Display
.


