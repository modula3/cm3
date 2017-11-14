(* ----------------------------------------------------------------------1- *)
(* File RefInst.i3 for Modula3 compiler test p031                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE RefInst

; TYPE ParamTyp1 = REFANY
; CONST ParamBits = BITSIZE ( ParamTyp1 ) 
  (* ^ 128 gives multiple compile errors, starting with:
     Could not find a legal alignment for the packed type. *) 
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Reference"

; VAR Val1 : ParamTyp1  
; VAR Val2 : ParamTyp1 

; END RefInst
. 
