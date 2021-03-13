(* ----------------------------------------------------------------------1- *)
(* File Dim3.ig for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE Dim3 ( InnerDim ) 
(* Used to instantiate Checks. *)

; CONST Depth = 3

; TYPE EltTyp = InnerDim . EltTyp
; CONST FullShape = InnerDim . FullShape
; CONST ShapeStart = NUMBER ( FullShape ) - Depth 
; CONST Number = FullShape [ ShapeStart ] 
; TYPE SsTyp = [ 0 .. Number - 1 ]
; TYPE DimSubsTyp = ARRAY [ 1 .. Depth ] OF INTEGER 
; TYPE OpenTyp = ARRAY OF InnerDim . OpenTyp 
; TYPE FixedTyp = ARRAY SsTyp OF InnerDim . FixedTyp

; PROCEDURE EltVal ( READONLY A : OpenTyp ; Subs : DimSubsTyp ) : EltTyp
; PROCEDURE SetEltVal ( VAR A : OpenTyp ; Subs : DimSubsTyp ; EltVal : EltTyp ) 
  
; END Dim3
.
