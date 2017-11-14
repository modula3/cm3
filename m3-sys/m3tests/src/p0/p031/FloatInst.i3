(* ----------------------------------------------------------------------1- *)
(* File FloatInst.i3 for Modula3 compiler test p031                         *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE FloatInst

; TYPE ParamTyp1 = REAL
; CONST ParamBits = BITSIZE(REAL)
  (* ^ 64 crashes front end on 64-bit machine:
       ** INTERNAL CG ERROR *** store_indirect size too large
       same for load_iondirect. *)
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Float" 
; CONST Val1 = 17.1
; CONST Val2 = 5.1  

; END FloatInst
. 
