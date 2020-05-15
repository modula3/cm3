GENERIC MODULE ArrayModes (Actuals , Formals )

(* Types declares:
   Label: TEXT (* used to identify what is tested. *)
   Types . MainType: a type to be passed.
   PackedType: BITS n FOR Types . MainType,
*)

; FROM Common IMPORT Case , Failure

; VAR Label = Actuals . Label & Formals . Label 

; PROCEDURE ByVALUE
    ( VALUE ValForm : Formals . Type ; ActAddr : ADDRESS ) : Formals . FixedType 
  ; BEGIN
    ; IF ActAddr # NIL
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
  ; VAR LocVal : REF Actuals . Type 
  ; BEGIN
      LocVal := ( NEW Actuals . Type )
    ; LocVal ^ := Formals . Val1 
    ; Result := ByValue ( LocVal ^ , ADR ( LocVal ^ ) ) 
    ; IsEqual := Types . Equal ( LocV1 , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", VALUE, expected " & Types . Image ( LocV1 )
            & ", got " & Types . Image ( Result )
          )  
      END (* IF *)
    END TestVALUE  

; PROCEDURE ByVAR
    ( VAR VarForm : Formals . Type
    ; Exp : Formals . FixedType
    ; ChangeTo : Formals . Type
    ; ActAddr : ADDRESS
    )
  = VAR IsEqual : BOOLEAN 
  ; BEGIN
      IsEqual := Formals . Equal ( VarForm , Exp )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", VAR, expected " & Formals . Image ( Exp )
            & ", got " & Formals . Image ( VarForm )
          )  
      END (* IF *) 
    ; Case ( ) 
    ; IF ADR ( VarForm ) # ActAddr 
      THEN
        Failure
          ( Label & ", VAR, formal and actual at different locations." )
      END (* IF *)
    ; VarForm := ChangeTo 
    END ByVAR

; PROCEDURE TestVAR ( )
  = VAR Address : ADDRESS
  ; VAR LocVal : REF Actuals . Type 
  ; BEGIN
      LocVal := ( NEW Actuals . Type )
    ; LocVal ^ := Formals . Val1 
    ; Address := ADR ( LocVal^ )
    ; ByVAR ( LocVal ^ , Formals . Val1 , ADR ( LocVal ^ ) , "designator" ) 
    ; Case ( ) 
    ; IF LocVal ^ # Formals . Val2
      THEN
        Failure
          ( Label & ", VAR, expected change to "
            & Types . Image ( Types . Val2 )
            & ", got " & Types . Image ( Types . Val1 )
          )  
      END (* IF *)
    END TestVAR

; PROCEDURE ROByRef 
    ( READONLY VarForm : Formals . Type
    ; ExpectedVal : Formals . Type
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
          ( Label & ", REDONLY by ref, " & Msg & " expected "
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
            ( Label & ", VAR, formal and actual at different locations." )
        END (* IF *)
      END (* IF *)
    END ROByRef

; TYPE CallerProc = PROCEDURE ( Msg : TEXT ) 
; TYPE ValCallerProc = PROCEDURE ( Msg : TEXT )  : Types . FixedType  

; PROCEDURE CallRODesig ( ) 
  = VAR LocVal : REF Actuals . Type 
  ; BEGIN
      LocVal := ( NEW Actuals . Type )
    ; LocVal ^ := Formals . Val1 
    ; ROByRef
        ( LocVal ^ , Types . Val1 , ADR ( LocVal ^ ) , "designator" ) 
    END CallRODesig 

; PROCEDURE CallROAnon ( ) 
  = VAR LocVal : Formals . Type 
  ; BEGIN
      LocVal := Types . Val1
    ; ROByRef
        ( Actuals . Type
            { Formals . Val1 [ 0 ] 
              Formals . Val1 [ 1 ] 
              Formals . Val1 [ 2 ] 
            } 
        , Types . Val1 , NIL , "single-use anonymous"
        ) 
    END CallROAnon 
 
; PROCEDURE CallRONonDesig ( ) 
  = CONST 
      LocVal = Actuals . Type
                 { Formals . Val1 [ 0 ] 
                   Formals . Val1 [ 1 ] 
                   Formals . Val1 [ 2 ] 
                 } 
  ; BEGIN
      ROByValue
        ( LocVal 
        , Types . Val1 , NIL , "single-use anonymous"
        ) 
    END CallRONonDesig 
 
; PROCEDURE TestROByRef ( Caller : CallerProc ; Msg : TEXT ) 

  = BEGIN
      Caller ( Msg )
    END TestROByRef

; PROCEDURE ROByValue
    ( READONLY ValForm : Formals . Type ; ActAddr : ADDRESS ; Msg : TEXT )
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
    ; IsEqual := Formals . Equal ( Types . Val1 , Result )
    ; Case ( ) 
    ; IF NOT IsEqual
      THEN
        Failure
          ( Label & ", READONLY by value, " & Msg
            & ", expected " & Types . Image ( Types . Val1 )
            & ", got " & Types . Image ( Result )
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
  END Modes
.

