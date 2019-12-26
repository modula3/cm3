(* ----------------------------------------------------------------------1- *)
(* File Lib.i3 for Modula3 compiler test p270                               *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Lib 

; TYPE Rng = [ 0 .. 15 ]

; TYPE Arr = ARRAY [ 0 .. 3 ] OF Rng 

; CONST GC = Arr { 10 , 25 , 16 , 9 }

; END Lib
.

