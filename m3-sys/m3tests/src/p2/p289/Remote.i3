(* ----------------------------------------------------------------------1- *)
(* File Remote.i3 for Modula3 compiler test p289                            *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)
INTERFACE Remote

; TYPE Arr = ARRAY [ 0 .. 1 ] OF INTEGER

(* Dummy constants surrounding the real ConstVal to catch addrssing errors. *)
; CONST Dummy1 = Arr { 1 , 3 } 

; CONST Dummy2 = Arr { 5 , 7 } 

; CONST ConstVal = Arr { 9 , 11 } 

; CONST Dummy3 = Arr { 25 , 27 } 

; CONST Dummy4 = Arr { 29 , 31 } 

; VAR VarVal := Arr { 13 , 15 } 

; END Remote
.

