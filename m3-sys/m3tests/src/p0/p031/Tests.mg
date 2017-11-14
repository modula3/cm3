(* ----------------------------------------------------------------------1- *)
(* File Tests.mg for Modula3 compiler test p031                             *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Tests ( ParamInfo )   

; FROM Support IMPORT WL
; FROM ParamInfo IMPORT ParamTyp1 , ParamTyp2 , Val1 , Val2 , Name 

; TYPE RecOfT2 = RECORD Fld : ParamTyp2 END
; VAR (* WONT CHANGE *) ValT1 : ParamTyp1 := Val1 
; VAR (* WONT CHANGE *) ValT1Mod : ParamTyp1 := Val2 
; VAR (* WONT CHANGE *) ValRecOfT2 := RecOfT2 { ValT1 }

; VAR GFailures : BOOLEAN := FALSE  

; PROCEDURE Check ( Actual , Expected : ParamTyp1 ; Msg : TEXT := "." )
  = BEGIN
      IF Actual = Expected THEN
        WL ( "    As expected" & Msg ) 
      ELSE
        WL ( "    NOT AS EXPECTED" & Msg )
      ; GFailures := TRUE 
      END (* IF *)
    END Check 

; PROCEDURE PT1VALUE ( VALUE Form : ParamTyp1 )
  = BEGIN
      WL ( "    In PT1VALUE" )
    ; Form := ValT1Mod
    END PT1VALUE 

; PROCEDURE PT1VAR ( VAR Form : ParamTyp1 )
  = BEGIN
      WL ( "    In PT1VAR" ) 
    ; Form := ValT1Mod
    END PT1VAR 

; PROCEDURE PT1RO ( READONLY Form : ParamTyp1 ; VAR Alias : ParamTyp1 ) : ParamTyp1 
  = BEGIN
      WL ( "    In PT1RO" ) 
    ; Alias := ValT1Mod
    ; RETURN Form
    END PT1RO 

; PROCEDURE PT1ROT2 ( READONLY Form : ParamTyp1 ; VAR Alias : ParamTyp2 ) : ParamTyp1 
  = BEGIN
      WL ( "    In PT1ROT2" ) 
    ; Alias := ValT1Mod
    ; RETURN Form
    END PT1ROT2 

; PROCEDURE RetT1Nondesig ( Form : ParamTyp1 ) : ParamTyp1
  (* Turn formal into a nondesignator. *) 
  = BEGIN
      RETURN Form 
    END RetT1Nondesig

; PROCEDURE Work ( VAR Failures : BOOLEAN ) 

  = VAR LVT1A , LVT1B : ParamTyp1 
  ; VAR LVRecOfT2 : RecOfT2
  ; BEGIN
      GFailures := FALSE 

    (* By VALUE: *)
    ; WL ( "Parameters of " & Name & " type:" ) 
    ; WL ( "  VALUE mode:" ) 
    ; LVT1A := ValT1
    ; PT1VALUE ( LVT1A ) 
    ; Check ( LVT1A , ValT1 ) 

    (* By reference: *) 
    ; WL ( "  VAR mode:" ) 
    ; LVT1A := ValT1
    ; PT1VAR ( LVT1A ) 
    ; Check ( LVT1A , ValT1Mod ) 

    (* READONLY, same type, designator: *) 
    ; WL ( "  READONLY mode, same type, designator (pass by reference):" ) 
    ; LVT1A  := ValT1
    ; LVT1B := PT1RO ( LVT1A , LVT1A )
      (* Pass by reference, result should be changed through its alias. *) 
    ; Check ( LVT1A , ValT1Mod , " through alias." ) 
    ; Check ( LVT1B , ValT1Mod , " through by-ref formal." ) 

    (* READONLY, same type, nondesignator: *) 
    ; WL ( "  READONLY mode, same type, nondesignator (copy and pass by reference):" ) 
    ; LVT1A  := ValT1
    ; LVT1B := PT1RO ( RetT1Nondesig ( LVT1A ) , LVT1A ) 
      (* Should be passed by value, result should be unchanged. *) 
    ; Check ( LVT1A , ValT1Mod , " through alias." ) 
    ; Check ( LVT1B , ValT1 , " through value formal." ) 

    (* READONLY, different type, designator: *) 
    ; WL ( "  READONLY mode, different type, designator (copy and pass by reference):" )
    ; LVT1A  := ValT1
    ; LVRecOfT2  := ValRecOfT2
    ; LVT1B := PT1ROT2 ( LVRecOfT2 . Fld , LVRecOfT2 . Fld ) 
      (* Should be passed by value, result should be unchanged. *) 
    ; Check ( LVRecOfT2 . Fld, ValT1Mod , " through alias." ) 
    ; Check ( LVT1B , ValT1 , " through value formal." )

    ; IF GFailures
      THEN
        WL ( "  Some " & Name & " cases ###FAILED###" ) 
      END (* IF *) 

    ; Failures := Failures OR GFailures 

    END Work

; BEGIN
  END Tests
. 
