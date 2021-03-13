(* ----------------------------------------------------------------------1- *)
(* File Checks0.ig for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE Checks0 ( Elts )

; IMPORT Common 

; PROCEDURE ChangeShapeHelper 
    ( VAR FromSubs , ToSubs : Common . FullSubsTyp
    ; FromShape , ToShape : ARRAY OF INTEGER 
    ; EltProc : CopyProcTyp
    ) 
    
; PROCEDURE ChangeShape 
    ( READONLY From : Elts . EltTyp 
    ; VAR To : Elts . EltTyp 
    ; READONLY FromShape , ToShape : ARRAY OF INTEGER
    )

; PROCEDURE CheckNumber
    ( Arr : Elts . EltTyp ; READONLY Shape : ARRAY OF INTEGER )

; TYPE EltProcTyp = PROCEDURE ( Subs : Common . FullSubsTyp ) : Elts . EltTyp 
; TYPE AssignEltProcTyp
    = PROCEDURE ( Subs : Common . FullSubsTyp ; Val : Elts . EltTyp ) 
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
    ( Arr : Elts . EltTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; FlatSs : INTEGER := 0 (* Use zero for top-level. *)
    ) 

; PROCEDURE FillFlatA
    ( VAR FlatVals : ARRAY OF INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; EltVal : INTEGER
    )

; PROCEDURE FillEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ; EltProc : AssignEltProcTyp 
    ) 

; PROCEDURE FillEltVals
    ( VAR Arr : Elts . EltTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ) 

; END Checks0
.
