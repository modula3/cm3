(* ----------------------------------------------------------------------1- *)
(* File SetsInst.i3 for Modula3 compiler test p031                          *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE SetsInst

; TYPE Range = [ 0 .. 31 ] 
; TYPE ParamTyp1 = SET OF Range
; CONST ParamBits = 64
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Set" 
; CONST Val1 = ParamTyp1 { 0 , 7 , 19 , 26 , 30 } 
; CONST Val2 = ParamTyp1 { 5 } 

; END SetsInst
. 
