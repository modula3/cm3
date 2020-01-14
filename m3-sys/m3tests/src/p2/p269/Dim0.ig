(* ----------------------------------------------------------------------1- *)
(* File Dim0.ig for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE Dim0 ( Elts , Shape ) 
(* Used to instantiate Checks. *)

; CONST Depth = 0

; TYPE EltTyp = Elts . EltTyp
; CONST FullShape = Shape . FullShape

; CONST ShapeStart = NUMBER ( FullShape ) (* Out-of-bounds. *)
; CONST Number = 0 
; CONST ShapeVal = ARRAY OF INTEGER { } 
; TYPE SsTyp = [ 0 .. - 1 ]
; TYPE DimSubsTyp = ARRAY [ 1 .. Depth ] OF INTEGER 
; TYPE OpenTyp = EltTyp 
; TYPE FixedTyp = EltTyp

; PROCEDURE EltVal ( READONLY A : OpenTyp ; Subs : DimSubsTyp ) : EltTyp
; PROCEDURE SetEltVal ( VAR A : OpenTyp ; Subs : DimSubsTyp ; EltVal : EltTyp ) 

; END Dim0
.
