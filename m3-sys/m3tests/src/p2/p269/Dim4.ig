(* ----------------------------------------------------------------------1- *)
(* File Dim4.ig for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE Dim4 ( InnerDim ) 
(* Used to instantiate Checks. *)

; CONST Depth = 4

; TYPE EltTyp = InnerDim . EltTyp
; CONST FullShape = InnerDim . FullShape
; CONST ShapeStart = NUMBER ( FullShape ) - Depth 
; CONST Number = FullShape [ ShapeStart ] 
; TYPE SsTyp = [ 0 .. Number - 1 ]
; TYPE DimSubsTyp = ARRAY [ 1 .. Depth ] OF INTEGER 
; TYPE OpenTyp = ARRAY OF InnerDim . OpenTyp 
; TYPE FixedTyp = ARRAY SsTyp OF InnerDim . FixedTyp

; PROCEDURE SetEltVal ( VAR A : OpenTyp ; Subs : DimSubsTyp ; EltVal : EltTyp ) 
; PROCEDURE EltVal ( READONLY A : OpenTyp ; Subs : DimSubsTyp ) : EltTyp
  
; END Dim4
.
