(* ----------------------------------------------------------------------1- *)
(* File Checks.ig for Modula3 compiler test p269                            *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE Checks ( Dim )
(* Dim must declare: 
     Depth       : INTEGER  (* Number of dimensions. *)
     EltTyp      = Type of non-array elements,  Assignable to INTEGER
     Number      : INTEGER, static element count.
     FullShape   : ARRAY OF INTEGER
     ShapeStart  : INTEGER (* Subscript to LM meaningful element of FullShape. *)
     SsTyp       = Type of subscripts. 
     OpenTyp     = Depth-dimensional open array type of EltTyp
     FixedTyp    = Depth-dimensional fixed array type of EltTyp
        When Depth = 0, OpenTyp and FixedType are EltTyp
        Dim.Depth must be Inner.Depth + 1
*)

; IMPORT Common 

; TYPE EltTyp = Dim . EltTyp 
; TYPE SsTyp = Dim . SsTyp 
; TYPE OpenTyp = Dim . OpenTyp
; TYPE FixedTyp = Dim . FixedTyp
; CONST Depth = Dim . Depth
; CONST Number = Dim . Number

; VAR ShapeRef : REF ARRAY OF INTEGER 

; PROCEDURE JustAccept ( READONLY Arr : OpenTyp )

; PROCEDURE ChangeShapeHelper
    ( VAR FromSubs , ToSubs : Common . FullSubsTyp
    ; FromShape , ToShape : ARRAY OF INTEGER 
    ; EltProc : CopyProcTyp
    ) 

; PROCEDURE ChangeShape 
    ( READONLY From : OpenTyp
    ; VAR To : OpenTyp
    ; FromShape , ToShape : ARRAY OF INTEGER
    )

; PROCEDURE CheckNumber
    ( READONLY Arr : OpenTyp ; READONLY Shape : ARRAY OF INTEGER )

; TYPE EltProcTyp = PROCEDURE ( Subs : Common . FullSubsTyp ) : Dim . EltTyp 
; TYPE AssignEltProcTyp
    = PROCEDURE ( Subs : Common . FullSubsTyp ; Val : Dim . EltTyp ) 
; TYPE CopyProcTyp
    = PROCEDURE ( READONLY FromSubs , ToSubs : Common . FullSubsTyp ) 

; PROCEDURE CheckEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; EltProc : EltProcTyp 
    ) 

; PROCEDURE CheckEltVals
    ( READONLY Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ) 

; PROCEDURE FillFlatA
    ( VAR FlatVal : ARRAY OF INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; Bias : INTEGER
    )
  (* Fill FlatVals using an algorithm. *)

; PROCEDURE FillEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ; EltProc : AssignEltProcTyp 
    ) 

; PROCEDURE FillEltVals
    ( VAR Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ) 

; PROCEDURE CheckDopeShape
    ( ShapeAddr : ADDRESS ; READONLY ExpShape : ARRAY OF INTEGER )

; PROCEDURE CheckConstOpenness
    ( READONLY OpenValue : Dim . OpenTyp
    ; READONLY FixedValue : Dim . FixedTyp
    ; READONLY ExpShape : ARRAY OF INTEGER
    ; ExpModuleName : TEXT 
    ; ExpReps : Common . RepSetTyp
    )

; PROCEDURE Check
    ( READONLY Arr : OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    )

; PROCEDURE CheckConstFixed
    ( READONLY Arr : FixedTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    ; ExpModuleName : TEXT 
    ; ExpReps : Common . RepSetTyp
    )

; PROCEDURE CheckConstOpen
    ( READONLY Arr : OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    ; ExpModuleName : TEXT 
    ; ExpReps : Common . RepSetTyp
    )

; END Checks
. 
