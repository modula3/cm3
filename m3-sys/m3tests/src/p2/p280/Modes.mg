(* ----------------------------------------------------------------------1- *)
(* File Modes.mg for Modula3 compiler test p280                             *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Modes (Types)

(* Types declares:
   Label: TEXT (* used to identify what is tested. *)
   Types . MainType: a type to be passed.
   PackedType: BITS n FOR Types . MainType,
*)

; FROM Common IMPORT Case , Failure

; TYPE ShortRec
    = RECORD TheField : Types . ShortType ; RightPad : Types . PadType END
; TYPE MisalignedRec
    = RECORD
        LeftPad : Types . PadType
      ; TheField : Types . BytesType
      ; RightPad : Types . PadType
      END 

; PROCEDURE ByValue
    ( VALUE ValForm : Types . MainType ; ActAddr : ADDRESS ) : Types . MainType 
  = BEGIN
      IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( ValForm ) = ActAddr 
        THEN
          Failure
            ( Types . Label & ", VALUE, actual has not been copied." )
        END (* IF *) 
      END (* IF *) 
    ; RETURN ValForm 
    END ByValue

; PROCEDURE TestVALUE ( )
  = VAR IsEqual : BOOLEAN 
  ; VAR LocV1 , Result : Types . MainType
  ; VAR Short : ShortRec 
  ; VAR Misaligned : MisalignedRec 
  ; BEGIN
    (* Unpacked value: *)
      LocV1 := Types . Val1
    ; Result := ByValue ( LocV1 , ADR ( LocV1 ) ) 
    ; IsEqual := Types . Equal ( LocV1 , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Types . Label & ", VALUE, expected " & Types . Image ( LocV1 )
            & ", got " & Types . Image ( Result )
          )  
      END (* IF *)

    (* Misaligned value: *)
    ; IF Types . DoPacked
      THEN
        Misaligned . LeftPad := FIRST ( Types . PadType )  
      ; Misaligned . TheField := Types . Val1
      ; Misaligned . RightPad := FIRST ( Types . PadType )  
      ; Result := ByValue ( Misaligned . TheField , NIL ) 
      ; IsEqual := Types . Equal ( Misaligned . TheField , Result )
      ; Case ( ) 
      ; IF NOT IsEqual
        THEN
          Failure
            ( Types . Label & ", VALUE, misaligned, expected "
              & Types . Image ( LocV1 )
              & ", got " & Types . Image ( Result )
            )  
        END (* IF *)
      END (* IF *)

    (* Packed value: *)
    ; IF Types . DoPacked
      THEN
        Short . TheField := Types . Val1
      ; Short . RightPad := FIRST ( Types . PadType )  
      ; Result := ByValue ( Short . TheField , ADR ( Short . TheField ) ) 
      ; IsEqual := Types . Equal ( Short . TheField , Result )
      ; Case ( ) 
      ; IF NOT IsEqual
        THEN
          Failure
            ( Types . Label & ", VALUE, packed 25, expected "
              & Types . Image ( LocV1 )
              & ", got " & Types . Image ( Result )
            )  
        END (* IF *)
      END (* IF *)
    END TestVALUE  

; PROCEDURE ByVar
    ( VAR VarForm : Types . MainType ; Exp : Types . MainType
    ; ChangeTo : Types . MainType ; ActAddr : ADDRESS
    )
  = VAR IsEqual : BOOLEAN 
  ; BEGIN
      IsEqual := Types . Equal ( VarForm , Exp )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Types . Label & ", VAR, expected " & Types . Image ( Exp )
            & ", got " & Types . Image ( VarForm )
          )  
      END (* IF *) 
    ; Case ( ) 
    ; IF ADR ( VarForm ) # ActAddr 
      THEN
        Failure
          ( Types . Label & ", VAR, formal and actual at different locations." )
      END (* IF *)
    ; VarForm := ChangeTo 
    END ByVar

; PROCEDURE TestVAR ( )
  = VAR Address : ADDRESS
  ; VAR LocV1 : Types . MainType 
  ; BEGIN
      LocV1 := Types . Val1
    ; Address := ADR ( LocV1 )
    ; ByVar ( LocV1 , Types . Val1 , Types . Val2 , Address )
    ; Case ( ) 
    ; IF LocV1 # Types . Val2
      THEN
        Failure
          ( Types . Label & ", VAR, expected change to "
            & Types . Image ( Types . Val2 )
            & ", got " & Types . Image ( Types . Val1 )
          )  
      END (* IF *)
    END TestVAR

; PROCEDURE MakeAnon ( VAR Val : Types . MainType ) : Types . MainType
  = BEGIN
      RETURN Val 
    END MakeAnon 

; <* UNUSED *> PROCEDURE AddrOfAnon ( READONLY Val : Types . MainType ) : ADDRESS 
  = BEGIN
      RETURN ADR ( Val ) 
    END AddrOfAnon 

; PROCEDURE ROByRef
    ( READONLY VarForm : Types . MainType
    ; ExpectedVal : Types . MainType
    ; ActAddr : ADDRESS
    ; Msg : TEXT 
    )
  = VAR IsEqual : BOOLEAN 
  ; BEGIN
      IsEqual := Types . Equal ( VarForm , ExpectedVal )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Types . Label & ", REDONLY by ref, " & Msg & " expected "
            & Types . Image ( ExpectedVal )
            & ", got " & Types . Image ( VarForm )
          )  
      END (* IF *) 
    ; IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( VarForm ) # ActAddr 
        THEN
          Failure
            ( Types . Label & ", VAR, formal and actual at different locations." )
        END (* IF *)
      END (* IF *)
    END ROByRef

; TYPE CallerProc = PROCEDURE ( Msg : TEXT ) 
; TYPE ValCallerProc = PROCEDURE ( Msg : TEXT )  : Types . MainType  

; <* UNUSED *> PROCEDURE CallSameTypeDesignator ( ) 
  = VAR LocVal : Types . MainType (* Which is not packed. *) 
  ; BEGIN
      LocVal := Types . Val1
    ; ROByRef
        ( LocVal , Types . Val1 , ADR ( LocVal ) , "same type designator" ) 
    END CallSameTypeDesignator 

; <* UNUSED *> PROCEDURE CallSameTypeAnon ( ) 
  = VAR LocVal : Types . MainType (* Which is not packed. *) 
  ; BEGIN
      LocVal := Types . Val1
    ; ROByRef
        ( MakeAnon ( LocVal )
        , Types . Val1 , NIL , "same type, single-use anonymous"
        ) 
    END CallSameTypeAnon 
 
; PROCEDURE TestROByRef ( Caller : CallerProc ; Msg : TEXT ) 

  = BEGIN
      Caller ( Msg )
    END TestROByRef

; PROCEDURE ROByValue
    ( READONLY ValForm : Types . MainType ; ActAddr : ADDRESS ; Msg : TEXT )
  : Types . MainType 
  = BEGIN
      IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( ValForm ) = ActAddr 
        THEN
          Failure
            ( Types . Label
              & ", READONLY by value, " & Msg & ", actual has not been copied."
            )
        END (* IF *) 
      END (* IF *) 
    ; RETURN ValForm 
    END ROByValue

; <* UNUSED *> PROCEDURE ROByValuePackedForm
    ( READONLY ValForm : Types . ShortType ; ActAddr : ADDRESS ; Msg : TEXT )
  : Types . MainType 
  = BEGIN
      IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( ValForm ) = ActAddr 
        THEN
          Failure
            ( Types . Label
              & ", READONLY, packed, by value, " & Msg & ", actual has not been copied."
            )
        END (* IF *) 
      END (* IF *) 
    ; RETURN ValForm 
    END ROByValuePackedForm

; PROCEDURE CallSameUnpackedTypeDesig ( Msg : TEXT ) 
  = VAR LocVal : Types . MainType
  ; BEGIN
      LocVal := Types . Val1
    ; ROByRef ( LocVal , LocVal , ADR ( LocVal ) , Msg ) 
    END CallSameUnpackedTypeDesig 

; PROCEDURE CallSameUnpackedTypeAnon ( Msg : TEXT ) 
  = VAR LocVal : Types . MainType
  ; BEGIN
      LocVal := Types . Val1
    ; ROByRef ( MakeAnon ( LocVal ) , LocVal , NIL , Msg ) 
    END CallSameUnpackedTypeAnon  

; PROCEDURE CallDiffTypeDesig ( Msg : TEXT ) : Types . MainType
  = VAR LocVal : Types . BytesType
  ; VAR Result : Types . MainType 
  ; BEGIN
      LocVal := Types . Val1
    ; Result := ROByValue ( LocVal , ADR ( LocVal ) , Msg ) 
    ; RETURN Result 
    END CallDiffTypeDesig 

; PROCEDURE CallShortTypeDesig ( Msg : TEXT ) : Types . MainType
  = VAR Result : Types . MainType 
  ; VAR Short : ShortRec 
  ; BEGIN
    (* Short value: *)
      IF Types . DoPacked
      THEN
        Short . TheField := Types . Val1
      ; Short . RightPad := FIRST ( Types . PadType )  
      ; Result
          := ROByValue
               ( Short . TheField , ADR ( Short . TheField ) , Msg )
      ELSE Result := Types.Val1
      END (* IF *)
    ; RETURN Result 
    END CallShortTypeDesig 

; PROCEDURE CallMisalignedDesig ( Msg : TEXT ) : Types . MainType
  = VAR Result : Types . MainType 
  ; VAR Misaligned : MisalignedRec 
  ; BEGIN
    (* Misaligned value: *)
      IF Types . DoPacked
      THEN
        Misaligned . LeftPad := FIRST ( Types . PadType )  
      ; Misaligned . TheField := Types . Val1
      ; Misaligned . RightPad := FIRST ( Types . PadType )  
      ; Result
          := ROByValue
               ( Misaligned . TheField , NIL , Msg )
      ELSE Result := Types.Val1
      END (* IF *)
    ; RETURN Result 
    END CallMisalignedDesig 

; PROCEDURE CallPackedTypeDesig ( Msg : TEXT ) : Types . MainType
  = VAR LocVal : Types . ShortType
  ; VAR Result : Types . MainType
  ; BEGIN
      LocVal := Types . Val1
    ; Result := ROByValue ( LocVal , ADR ( LocVal ) , Msg ) 
    ; RETURN Result 
    END CallPackedTypeDesig 

; PROCEDURE CallSameUnpackedTypeNondesig ( Msg : TEXT ) : Types . MainType
  = VAR Result : Types . MainType
  ; BEGIN
      Result := ROByValue ( Types.NondesigVal1 , NIL , Msg ) 
    ; RETURN Result 
    END CallSameUnpackedTypeNondesig 

; PROCEDURE TestROByValue ( Caller : ValCallerProc ; Msg : TEXT ) 

  = VAR IsEqual : BOOLEAN
  ; VAR Result : Types . MainType 
  
  ; BEGIN
    (* Unpacked value: *)
      Result := Caller ( Msg ) 
    ; IsEqual := Types . Equal ( Types . Val1 , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Types . Label & ", READONLY by value, " & Msg
            & ", expected " & Types . Image ( Types . Val1 )
            & ", got " & Types . Image ( Result )
          )  
      END (* IF *)
    END TestROByValue

; PROCEDURE TestREADONLY ( )
  = BEGIN
      TestROByRef
        ( CallSameUnpackedTypeDesig , "same nonpacked type, designator" ) 
    ; TestROByRef
        ( CallSameUnpackedTypeAnon , "same nonpacked type, anonymous" ) 
    ; TestROByValue
        ( CallDiffTypeDesig , "different types, designator" ) 
    ; TestROByValue
        ( CallMisalignedDesig , "misaligned, designator" ) 
    ; TestROByValue
        ( CallShortTypeDesig , "short actual, designator" ) 
    ; TestROByValue
        ( CallPackedTypeDesig , "packed type, designator" ) 
    ; TestROByValue
        ( CallSameUnpackedTypeNondesig , "same nonpacked type, nondesignator" ) 
    ; TestROByValue
        ( CallDiffTypeDesig , "different types, designator" )
    END TestREADONLY

; PROCEDURE TestAll ( )
  = BEGIN 
      TestVALUE ( ) 
    ; TestVAR ( ) 
    ; TestREADONLY ( )
    END TestAll
    
; BEGIN
  END Modes
.

