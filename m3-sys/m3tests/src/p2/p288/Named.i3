(* ----------------------------------------------------------------------1- *)
(* File Named.i3 for Modula3 compiler test p288                             *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Named

; IMPORT Display 

; CONST ExpA = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 8 , 15 }
; CONST ExpR = RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 9 , 16 }
; CONST ExpS = SET OF Display . Rng { 18 , 98 , 130 } 

; CONST ImpA = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 6 , 11 }
; CONST ImpR = RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 7 , 12 }
; CONST ImpS = SET OF Display . Rng { 16 , 96 , 128 } 

; CONST IntfA : Display . ArrT = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 4 , 7 }
; CONST IntfR : Display . RecT = RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 5 , 8 }
; CONST IntfS : Display . SetT = SET OF Display . Rng { 19 , 99 , 131 }


(* Default values named in self interface. *)
; TYPE Intf = RECORD
    A := IntfA 
  ; R := IntfR
  ; S := IntfS 
  END 

(* Second uses of self named defaults. *)
; TYPE Intf2 = RECORD
    A := IntfA 
  ; R := IntfR
  ; S := IntfS 
  END 

; END Named
.
