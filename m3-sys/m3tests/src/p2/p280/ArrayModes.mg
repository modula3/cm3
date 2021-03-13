(* ----------------------------------------------------------------------1- *)
(* File ArrayModes.mg for Modula3 compiler test p280                        *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE ArrayModes (Actuals , Formals )

(* Types declares:
   Label: TEXT (* used to identify what is tested. *)
   Types . MainType: a type to be passed.
   PackedType: BITS n FOR Types . MainType,
*)

; IMPORT Common 
; FROM Common IMPORT Case , Failure , Array1DOpen , Array2DOpen , Array3DOpen
; FROM Common IMPORT Equal3D , ArrayImage3D , StripDope   

; PROCEDURE ByVALUE
    ( VALUE ValForm : Formals . MainType ; ActAddr : ADDRESS )
    (* ^ Let this one produce warning. *)
  : Formals . FixedType 
  = BEGIN
      IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( ValForm ) = ActAddr 
        THEN
          Failure
            ( Label & ", VALUE, actual has not been copied." )
        END (* IF *) 
      END (* IF *) 
    ; RETURN ValForm 
    END ByVALUE

; PROCEDURE TestVALUE ( )
  = VAR IsEqual : BOOLEAN 
  ; VAR LocVal : REF Actuals . MainType
  ; VAR Result : Formals . FixedType  
  ; BEGIN
      LocVal := Actuals . NewRef ( ) 
    ; LocVal ^ := Formals . Val1 
    ; Result := ByVALUE ( LocVal ^ , ADR ( LocVal ^ ) ) 
    ; IsEqual := Equal3D ( LocVal ^ , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", VALUE, expected " & ArrayImage3D ( LocVal ^ )
            & ", got " & ArrayImage3D ( Result )
          )  
      END (* IF *)
    END TestVALUE

; PROCEDURE FormAddrs ( VAR Value : Formals . InnerType ; addr : ADDRESS )
(* Something is wrong with ADR ( OpenArrayVARFormal[0] *) 
  = VAR FormAddr : ADDRESS
  ; VAR RefAddr : ADDRESS
  ; VAR Bkpt : INTEGER 
  ; BEGIN
      FormAddr := ADR ( Value )
    ; RefAddr := addr
    ; Bkpt := 9
    END FormAddrs 

; PROCEDURE ByVAR
    ( VAR VarForm : Formals . MainType
    ; READONLY Exp : Formals . FixedType
    ; READONLY ChangeTo : Formals . MainType
    ; ActAddr : ADDRESS
    )
  = VAR FormAddr , FormEltsAddr : ADDRESS 
  ; VAR IsEqual : BOOLEAN 
  ; BEGIN
      IsEqual := Equal3D ( VarForm , Exp )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", VAR, expected " & ArrayImage3D ( Exp )
            & ", got " & ArrayImage3D ( VarForm )
          )  
      END (* IF *)
    ; IF ActAddr # NIL
      THEN
        Case ( )
      ; FormAddrs ( VarForm [ 0 ] , ADR ( VarForm [ 0 ] ) ) 
      ; FormAddr := ADR ( VarForm [ 0 ] ) 
      ; FormEltsAddr := StripDope ( FormAddr , NUMBER ( VarForm ) , Label & ", VAR ") 
      ; IF FormEltsAddr = NIL AND FormEltsAddr # ActAddr 
        THEN
          Failure
            ( Label & ", VAR, formal and actual at different locations." )
        END (* IF *)
      END 
    ; VarForm := ChangeTo 
    END ByVAR

; PROCEDURE TestVAR ( )
  = VAR Address : ADDRESS
  ; VAR LocVal : REF Actuals . MainType 
  ; BEGIN
      LocVal := Actuals . NewRef ( ) 
    ; LocVal ^ := Formals . Val1 
    ; Address := ADR ( LocVal ^ )
    ; ByVAR ( LocVal ^ , Formals . Val1 , Formals . Val2 , ADR ( LocVal ^ [ 0 ] ) )
    ; Case ( ) 
    ; IF LocVal ^ # Formals . Val2
      THEN
        Failure
          ( Label & ", VAR, expected change to "
            & ArrayImage3D ( Formals . Val2 )
            & ", got " & ArrayImage3D ( Formals . Val1 )
          )  
      END (* IF *)
    END TestVAR

; PROCEDURE ROByRef 
    ( READONLY VarForm : Formals . MainType
    ; READONLY ExpectedVal : Formals . MainType
    ; ActAddr : ADDRESS
    ; Msg : TEXT 
    )
  = VAR FormAddr , FormEltsAddr : ADDRESS 
  ; VAR IsEqual : BOOLEAN 
  ; BEGIN
      IsEqual := Equal3D ( VarForm , ExpectedVal )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", REDONLY by ref, " & Msg & " expected "
            & ArrayImage3D ( ExpectedVal )
            & ", got " & ArrayImage3D ( VarForm )
          )  
      END (* IF *) 
    ; IF ActAddr # NIL
      THEN
        Case ( ) 
      ; FormAddr := ADR ( VarForm [ 0 ] ) 
      ; FormEltsAddr
          := StripDope ( FormAddr , NUMBER ( VarForm ) , Label & ", READONLY by ref" ) 
      ; IF FormEltsAddr = NIL AND FormEltsAddr # ActAddr 
        THEN
          Failure
            ( Label & ", READONLY by ref, formal and actual at different locations." )
        END (* IF *)
      END (* IF *)
    END ROByRef

; TYPE CallerProc = PROCEDURE ( Msg : TEXT ) 
; TYPE ValCallerProc = PROCEDURE ( Msg : TEXT ) : Formals . FixedType  

; PROCEDURE CallRODesig ( Msg : TEXT ) 
  = VAR LocVal : REF Actuals . MainType 
  ; BEGIN
      LocVal := Actuals . NewRef ( ) 
    ; LocVal ^ := Formals . Val1 
    ; ROByRef
        ( LocVal ^ , Formals . Val1 , ADR ( LocVal ^ [ 0 ] ) , Msg & ", designator" ) 
    END CallRODesig 

; PROCEDURE CallROAnon ( Msg : TEXT ) 
  = BEGIN
      ROByRef
        ( Array3DOpen 
            {
              Array2DOpen 
                {
                  Array1DOpen { 1476332 , 11 , 12 }
                , Array1DOpen { 15 , 20 , 23 }
                , Array1DOpen { 37 , 51 , 72 }
                }
            , Array2DOpen 
                {
                  Array1DOpen { 6 , 24 , 90 }
                , Array1DOpen { 87 , 39 , 14 }
                , Array1DOpen { 21 , 1945 , 1776 }
                }
            , Array2DOpen 
                {
                  Array1DOpen { 1999 , 332 , 497 }
                , Array1DOpen { 100 , 10 , 0 }
                , Array1DOpen { 1919 , 3768 , 1401 }
                }
            }
        , Formals . Val1
        , NIL
        , Msg & ", single-use anonymous"
        ) 
    END CallROAnon 
 
; PROCEDURE CallRONondesig ( Msg : TEXT ) : Formals . FixedType 
  = CONST 
      LocVal 
        = Array3DOpen 
            {
              Array2DOpen 
                {
                  Array1DOpen { 1476332 , 11 , 12 }
                , Array1DOpen { 15 , 20 , 23 }
                , Array1DOpen { 37 , 51 , 72 }
                }
            , Array2DOpen 
                {
                  Array1DOpen { 6 , 24 , 90 }
                , Array1DOpen { 87 , 39 , 14 }
                , Array1DOpen { 21 , 1945 , 1776 }
                }
            , Array2DOpen 
                {
                  Array1DOpen { 1999 , 332 , 497 }
                , Array1DOpen { 100 , 10 , 0 }
                , Array1DOpen { 1919 , 3768 , 1401 }
                }
            }

  ; VAR Result : Formals . FixedType 
  ; BEGIN
      Result := ROByValue ( LocVal , NIL , Msg & ", single-use anonymous" )
    ; RETURN Result 
    END CallRONondesig 
 
; PROCEDURE TestROByRef ( Caller : CallerProc ; Msg : TEXT ) 

  = BEGIN
      Caller ( Msg )
    END TestROByRef

; PROCEDURE ROByValue
    ( READONLY ValForm : Formals . MainType ; ActAddr : ADDRESS ; Msg : TEXT )
  : Formals . FixedType 
  = BEGIN
      IF ActAddr # NIL
      THEN
        Case ( ) 
      ; IF ADR ( ValForm ) = ActAddr 
        THEN
          Failure
            ( Label
              & ", READONLY by value, " & Msg & ", actual has not been copied."
            )
        END (* IF *) 
      END (* IF *)
    ; RETURN ValForm 
    END ROByValue

; PROCEDURE TestROByValue ( ValCaller : ValCallerProc ; Msg : TEXT ) 

  = VAR IsEqual : BOOLEAN
  ; VAR Result : Formals . FixedType 
  
  ; BEGIN
      Result := ValCaller ( Msg ) 
    ; IsEqual := Equal3D ( Formals . Val1 , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", READONLY by value, " & Msg
            & ", expected " & ArrayImage3D ( Formals . Val1 )
            & ", got " & ArrayImage3D ( Result )
          )  
      END (* IF *)
    END TestROByValue

; PROCEDURE TestREADONLY ( )
  = BEGIN
      TestROByRef ( CallRODesig , ", designator" ) 
    ; TestROByRef ( CallROAnon , ", anonymous" ) 
    ; TestROByValue ( CallRONondesig , ", nondesignator" ) 
    END TestREADONLY

; PROCEDURE TestAll ( )
  = BEGIN 
      TestVALUE ( ) 
    ; TestVAR ( ) 
    ; TestREADONLY ( )
    END TestAll
    
; BEGIN
  END ArrayModes
.

