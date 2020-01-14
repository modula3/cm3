(* File Checks0.mg for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Checks0 ( Elts )
; IMPORT Fmt

; IMPORT Common
; FROM Common IMPORT SubsImage 
; IMPORT Globals 
; FROM Support IMPORT Note , NoteFailure  

(*EXPORTED*) 
; PROCEDURE ChangeShapeHelper
    ( VAR FromSubs , ToSubs : Common . FullSubsTyp
    ; <*UNUSED*> <*NOWARN*> FromShape , ToShape : ARRAY OF INTEGER 
    ; EltProc : CopyProcTyp
    ) 
  = BEGIN
      EltProc ( FromSubs , ToSubs )
    END ChangeShapeHelper

(*EXPORTED*) 
; PROCEDURE ChangeShape 
    ( <*UNUSED*> READONLY From : Elts . EltTyp 
    ; <*UNUSED*> VAR To : Elts . EltTyp 
    ; <*UNUSED*> READONLY FromShape , ToShape : ARRAY OF INTEGER
    )
  = BEGIN
      <* ASSERT FALSE *> (* Should never be called. *)
    END ChangeShape 

(*EXPORTED*) 
; PROCEDURE CheckNumber
    ( <*UNUSED*> Arr : Elts . EltTyp
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    )
  = BEGIN
      <* ASSERT FALSE *> (* Should never be called. *)
    END CheckNumber

(*EXPORTED*) 
; PROCEDURE CheckEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; EltProc : EltProcTyp 
    ) 
  = VAR EltVal , Exp : Elts . EltTyp
  ; BEGIN
      <* ASSERT NUMBER ( FlatVals ) = 1 *>
      EltVal := EltProc ( Subs )
    ; Exp := FlatVals [ 0 ] 
    ; IF EltVal # Exp
      THEN
        NoteFailure
          ( Globals . Name & SubsImage ( Subs , TopDepth )
            & " = " & Fmt . Int ( EltVal )
            & " , expected " & Fmt . Int ( Exp ) & "."
          )
      ELSIF Globals . GDisplaySuccess
      THEN 
        Note
          ( Globals . Name & SubsImage ( Subs , TopDepth )
            & " = " & Fmt . Int ( EltVal )
          )
      END (*IF*)
    END CheckEltValsHelper

(*EXPORTED*) 
; PROCEDURE CheckEltVals
    ( <*UNUSED*> Arr : Elts . EltTyp
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    ; <*UNUSED*> READONLY FlatVals : ARRAY OF INTEGER
    ; <*UNUSED*> FlatSs : INTEGER := 0 (* Use zero for top-level. *)
    ) 
  = BEGIN
      <* ASSERT FALSE *> (* Should never be called. *)
    END CheckEltVals

; PROCEDURE FillFlatA
    ( VAR FlatVals : ARRAY OF INTEGER 
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    ; EltVal : INTEGER
    )

  = BEGIN
      <* ASSERT NUMBER ( FlatVals ) = 1 *>
      FlatVals [ 0 ]  := EltVal 
    END FillFlatA 

(*EXPORTED*) 
; PROCEDURE FillEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; <*UNUSED*> TopDepth : INTEGER 
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; <*UNUSED*> Bias : INTEGER 
    ; EltProc : AssignEltProcTyp 
    ) 

  = BEGIN
      <* ASSERT NUMBER ( FlatVals ) = 1 *>
      EltProc ( Subs , FlatVals [ 0 ] ) 
    END FillEltValsHelper 

(*EXPORTED*) 
; PROCEDURE FillEltVals
    ( <*UNUSED*> VAR Arr : Elts . EltTyp
    ; <*UNUSED*> READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; <*UNUSED*> Bias : INTEGER 
    ) 
  = BEGIN
      <* ASSERT NUMBER ( FlatVals ) = 1 *>
      <* ASSERT FALSE *> (* Should never be called. *)
    END FillEltVals 


; BEGIN
  END Checks0
.
