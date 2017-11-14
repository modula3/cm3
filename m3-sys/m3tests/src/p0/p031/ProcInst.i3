(* ----------------------------------------------------------------------1- *)
(* File ProcInst.i3 for Modula3 compiler test p031                          *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE ProcInst

; TYPE ParamTyp1 = PROCEDURE ( ) RAISES ANY 
; CONST ParamBits = 64
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Procedure" 
; PROCEDURE Val1 ( ) RAISES ANY 
; PROCEDURE Val2 ( ) RAISES ANY 

; END ProcInst
. 
