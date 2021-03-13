(* 64-bit-sized but only 32-bit-aligned values, on 32-bit machines. *)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; VAR WrT : Wr . T

; PROCEDURE Int ( I : INTEGER ) : TEXT
  = BEGIN
      RETURN Fmt . Int ( I ) 
    END Int

; PROCEDURE EOL ( )
  = <*FATAL Thread.Alerted, Wr.Failure*>
    BEGIN
      Wr . PutText ( WrT , Wr . EOL )
    END EOL 

; PROCEDURE WL ( A , B , C , D : TEXT := NIL )
  = <*FATAL Thread.Alerted, Wr.Failure*>
    BEGIN
      IF A # NIL THEN Wr . PutText ( WrT , A ) END 
    ; IF B # NIL THEN Wr . PutText ( WrT , B ) END 
    ; IF C # NIL THEN Wr . PutText ( WrT , C ) END 
    ; IF D # NIL THEN Wr . PutText ( WrT , D ) END
    ; EOL ( ) 
    END WL

; PROCEDURE Report ( )
  = BEGIN
      WL ( Int ( TestCt ) , " tests performed." )
    ; IF FailureCt <= 0
      THEN
        WL ( "All succeeded." )
      ; WL ( "Overall test succeeded." )
      ELSE
        WL ( Int ( FailureCt ) , " of them failed." )
      ; WL ( "Overall test ##### FAILED #####." )
      END
    END Report

; VAR TestCt : INTEGER
; VAR FailureCt : INTEGER
; VAR Verbose := TRUE 

; TYPE T8 = RECORD Field : BITS 8 FOR [ 0 .. 255 ] END 

; VAR GLI : LONGINT := 0L

; PROCEDURE PLI ( VAR FLI : LONGINT ) : LONGINT 
  = <*UNUSED*> VAR Misalign : T8
  ; VAR LLI : LONGINT
  ; BEGIN
      LLI := FLI
    ; RETURN LLI 
    END PLI

; VAR GLC : LONGCARD := 0L

; PROCEDURE PLC ( VAR FLC : LONGCARD ) : LONGCARD 
  = <*UNUSED*> VAR Misalign : T8
  ; VAR LLC : LONGCARD
  ; BEGIN
      LLC := FLC
    ; RETURN LLC 
    END PLC

; VAR GLR : LONGREAL := 0.0D0

; PROCEDURE PLR ( VAR FLR : LONGREAL ) : LONGREAL 
  = <*UNUSED*> VAR Misalign : T8 
  ; VAR LLR : LONGREAL
  ; BEGIN
      LLR := FLR
    ; RETURN LLR 
    END PLR

; PROCEDURE PEx ( VAR FEx : EXTENDED ) : EXTENDED 
  = <*UNUSED*> VAR Misalign : T8 
  ; VAR LEx : EXTENDED
  ; BEGIN
      LEx := FEx
    ; RETURN LEx 
    END PEx

; VAR GEx : EXTENDED := 0.0X0

; PROCEDURE Test ( IsOK : BOOLEAN ; Label : TEXT )
  = BEGIN
      IF IsOK
      THEN
        IF Verbose
        THEN
          WL ( "Case " & Label & " succeeded." )
        END 
      ELSE 
        WL ( "Case " & Label & " ##### Failed #####." )
      ; INC ( FailureCt ) 
      END
    ; INC ( TestCt )
    END Test 

; BEGIN
    TestCt := 0
  ; FailureCt := 0
  ; WrT := Stdio . stdout
  ; Verbose := FALSE 

  ; Test ( PLI ( GLI ) = GLI , "PLI" ) 
  ; Test ( PLC ( GLC ) = GLC , "PLC" ) 
  ; Test ( PLR ( GLR ) = GLR , "PLR" ) 
  ; Test ( PEx ( GEx ) = GEx , "PEx" )

  ; Report ( ) 
  END Main
.
