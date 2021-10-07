(* ----------------------------------------------------------------------1- *)
(* File Named.i3 for Modula3 compiler test p287                             *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Named

; IMPORT Display 

; CONST ArrFC = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 14 , 27 } 
; CONST ArrOC = ARRAY OF INTEGER { 1 , 16 , 31 , 46 , 61 , 76 }

; CONST RecC = RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 9 , 16 }

; CONST SetC = SET OF Display . Rng { 18 , 98 , 130 } 

; END Named
.
