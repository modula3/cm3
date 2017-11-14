(* ----------------------------------------------------------------------1- *)
(* File RecInst.i3 for Modula3 compiler test p031                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE RecInst

; TYPE ParamTyp1 = RECORD F : [ 0 .. 31 ] := 27 END 
; CONST ParamBits = BITSIZE(ParamTyp1)
  (* ^ 64 crashes front end *)
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Record" 
; CONST Val1 = ParamTyp1 { 27 } 
; CONST Val2 = ParamTyp1 { 6 } 

; END RecInst
. 
