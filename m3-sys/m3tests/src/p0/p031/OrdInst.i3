(* ----------------------------------------------------------------------1- *)
(* File OrdInst.i3 for Modula3 compiler test p031                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE OrdInst

; TYPE Range = [ 0 .. 31 ] 
; TYPE ParamTyp1 = Range
; CONST ParamBits = 8
  (* ^ 64 crashes front end:
      ** INTERNAL CG ERROR *** unable to find integer type?  type=Word.64
         size/offset/align=64/0/8 *)
; TYPE ParamTyp2 = BITS ParamBits FOR ParamTyp1 
; CONST Name = "Ordinal" 
; CONST Val1 = 17
; CONST Val2 = 5  

; END OrdInst
. 
