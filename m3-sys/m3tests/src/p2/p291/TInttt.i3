(* ----------------------------------------------------------------------1- *)
(* File TInttt.i3 for Modula3 compiler test p291                            *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)
INTERFACE TInttt

; TYPE IByte = BITS 8 FOR [ 0 .. 16_FF ] 
; TYPE Int = ARRAY [ 0 .. 8] OF IByte
; CONST Val1 = Int { 13 , .. }
  (* ^ Just to force the constants below to have non-zero displacements within
       the global constant area. *) 
; CONST Val2 = Int { 20 , 21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 }
; CONST Val3 = Int { 30 , 31 , 32 , 33 , 34 , 35 , 36 , 37 , 38 }
; CONST Val4 = Int { 40 , 41 , 42 , 43 , 44 , 45 , 46 , 47 , 48 }
; CONST Lo2 = Val2 [ 0 ] 
; CONST Lo3 = Val3 [ 0 ] 
; CONST Lo4 = Val4 [ 0 ] 

; END TInttt
.

