(* ----------------------------------------------------------------------1- *)
(* File: TextForAllDo.m3, Modula-3 source code.                             *)
(* Copyright 2016, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test for Text.ForAllDo. *) 
                                                                               
UNSAFE MODULE TextForAllDo EXPORTS Main

; IMPORT Fmt
; IMPORT RTProcess
; IMPORT Stdio
; IMPORT Text
; IMPORT Text16
; IMPORT Text16Short
; IMPORT Text8
; IMPORT Text8Short
; IMPORT TextCat
; IMPORT TextClass
; IMPORT TextLiteral
; IMPORT TextSub
; IMPORT Thread 
; IMPORT Wr 

; <* FATAL Thread . Alerted *> 
  <* FATAL Wr . Failure *> 

  VAR GTestCt , GFailureCt : CARDINAL := 0 
; VAR WrT : Wr . T := Stdio . stdout 

; PROCEDURE T8S ( Val : TEXT ) : TEXT 

  = VAR LLen : CARDINAL 
  ; VAR LResult : TEXT 
  ; VAR LChars : ARRAY [ 0 .. Text8Short . MaxLength - 1 ] OF CHAR 

  ; BEGIN
      LLen := Text . Length ( Val ) 
    ; <*ASSERT LLen <= Text8Short . MaxLength *>
      Text . SetChars ( LChars , Val ) 
    ; LResult :=  Text8Short . New ( SUBARRAY ( LChars , 0 , LLen ) ) 
    ; RETURN LResult 
    END T8S

; PROCEDURE T16S ( Val : TEXT ) : TEXT 

  = VAR LLen : CARDINAL 
  ; VAR LResult : TEXT 
  ; VAR LChars : ARRAY [ 0 .. Text16Short . MaxLength - 1 ] OF WIDECHAR 

  ; BEGIN
      LLen := Text . Length ( Val ) 
    ; <*ASSERT LLen <= Text16Short . MaxLength *>
      Text . SetWideChars ( LChars , Val ) 
    ; LResult :=  Text16Short . New ( SUBARRAY ( LChars , 0 , LLen ) ) 
    ; RETURN LResult 
    END T16S

; PROCEDURE T8 ( Val : TEXT ) : TEXT 

  = VAR LLen : CARDINAL 
  ; VAR LResult : TEXT 
  ; VAR LCharsRef : REF ARRAY OF CHAR 

  ; BEGIN
      LLen := Text . Length ( Val ) 
    ; LCharsRef := NEW ( REF ARRAY OF CHAR , LLen ) 
    ; Text . SetChars ( LCharsRef^ , Val ) 
    ; LResult :=  Text8 . New ( LCharsRef ^ ) 
    ; RETURN LResult 
    END T8

; PROCEDURE T16 ( Val : TEXT ) : TEXT 

  = VAR LLen : CARDINAL 
  ; VAR LResult : TEXT 
  ; VAR LCharsRef : REF ARRAY OF WIDECHAR 

  ; BEGIN
      LLen := Text . Length ( Val ) 
    ; LCharsRef := NEW ( REF ARRAY OF WIDECHAR , LLen ) 
    ; Text . SetWideChars ( LCharsRef^ , Val ) 
    ; LResult :=  Text16 . New ( LCharsRef ^ ) 
    ; RETURN LResult 
    END T16

; CONST Flatten = T16

; PROCEDURE ForAll ( Val : TEXT ) : TEXT 

  = VAR LLen , LNext : CARDINAL 
  ; VAR LResult : TEXT 
  ; VAR LCharsRef : REF ARRAY OF WIDECHAR 

  ; PROCEDURE VisitChars ( READONLY ArrCh : ARRAY OF CHAR ) 

    = VAR LFragLen : CARDINAL 

    ; BEGIN 
        LFragLen := NUMBER ( ArrCh ) 
      ; FOR RI := 0 TO LAST ( ArrCh ) 
        DO
          LCharsRef ^ [ LNext + RI ] := ArrCh [ RI ] 
        END (* FOR *) 
      ; INC ( LNext , LFragLen ) 
      END VisitChars

  ; PROCEDURE VisitWideChars ( READONLY ArrCh : ARRAY OF WIDECHAR ) 

    = VAR LFragLen : CARDINAL 

    ; BEGIN 
        LFragLen := NUMBER ( ArrCh ) 
      ; SUBARRAY ( LCharsRef ^ , LNext , LFragLen ) := ArrCh 
      ; INC ( LNext , LFragLen ) 
      END VisitWideChars

  ; BEGIN (* ForAll *) 
      LLen := Text . Length ( Val ) 
    ; LCharsRef := NEW ( REF ARRAY OF WIDECHAR , LLen * 10 ) 
    ; FOR RI := 0 TO LAST ( LCharsRef ^ ) 
      DO LCharsRef ^ [ RI ] := '\x00'
      END (* FOR *) 
    ; LNext := 0 
    ; <* FATAL ANY *> 
      BEGIN 
        Text . ForAllDo ( Val , VisitChars , VisitWideChars ) 
      END (* Block *) 
    ; LResult := Text . FromWideChars ( SUBARRAY ( LCharsRef ^ , 0 , LNext ) ) 
    ; RETURN LResult 
    END ForAll 

; CONST MsgCol = 30

; PROCEDURE Check ( Val : TEXT ; Label : TEXT ) 

  = VAR LFlat , LForAll : TEXT 
  ; VAR LFailed : BOOLEAN := FALSE 

  ; BEGIN
      IF Text . Length ( Label ) < MsgCol 
      THEN 
        Wr . PutText 
          ( WrT 
          , Fmt . Pad ( Label , MsgCol , align := Fmt . Align . Left ) 
          ) 
      ELSE 
        Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( "" , MsgCol ) ) 
      END (* IF *) 

    ; LFlat := Flatten ( Val ) 
    ; IF NOT Text . Equal ( LFlat , Val ) 
      THEN 
        Wr . PutText ( WrT , " SetWideChars failed: \"" ) 
      ; Wr . PutText ( WrT , Val ) 
      ; Wr . PutText ( WrT , "\" became \"" ) 
      ; Wr . PutText ( WrT , LFlat ) 
      ; Wr . PutText ( WrT , "\"" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; INC ( GFailureCt ) 
      ; LFailed := TRUE 
      ; Wr . Flush ( WrT ) 
      END (* IF *) 

    ; LForAll := ForAll ( Val ) 
    ; IF NOT Text . Equal ( LForAll , Val ) 
      THEN 
        IF LFailed 
        THEN Wr . PutText ( WrT , Fmt . Pad ( "" , MsgCol ) ) 
        ELSE LFailed := TRUE 
        END
      ; Wr . PutText ( WrT , " ForAllDo failed: \"" ) 
      ; Wr . PutText ( WrT , Val ) 
      ; Wr . PutText ( WrT , "\" became \"" ) 
      ; Wr . PutText ( WrT , LForAll ) 
      ; Wr . PutText ( WrT , "\"" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; INC ( GFailureCt ) 
      ; Wr . Flush ( WrT ) 
      END (* IF *) 

    ; IF NOT LFailed 
      THEN
        Wr . PutText ( WrT , " yielded \"" ) 
      ; Wr . PutText ( WrT , LForAll ) 
      ; Wr . PutText ( WrT , "\", as expected." ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT ) 
      END (* IF *) 

    ; INC ( GTestCt ) 
    END Check 

; PROCEDURE Work ( ) 

  = VAR T1 , T2 , T3 , T4 , T5 : TEXT 
  ; VAR T6 , T7 , T8 , T9 , T10 : TEXT 
  ; VAR T11 , T12 , T13 , T14 , T15 : TEXT 
  ; VAR T16 , T17 , T18 , T19 , T20 : TEXT 

  ; BEGIN 
      T1 := "abc" 
    ; Check ( T1 , "\"abc\"") 

    ; T2 := W"ABC" 
    ; Check ( T2 , "W\"ABC\"") 

    ; T3 := "abcdefghijklmnopqrstuvwxyz"
    ; Check ( T3 , "\"abcdefghijklmnopqrstuvwxyz\"") 

    ; T4 := "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ; Check ( T4 , "W\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"") 

    ; T5 := T1 & T2 
    ; Check ( T5 , "\"abc\"&\"ABC\"") 

    ; T6 := T3 & T4 
    ; Check ( T6 , "\"abcdefghijklmnopqrstuvwxyz\"&\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"") 

    ; T7 := Text . Sub ( T5 , 1 , 1 ) 
    ; Check ( T7 , "Sub(\"abcABC\",1,1)" )

    ; T8 := Text . Sub ( T5 , 1 , 2 ) 
    ; Check ( T8 , "Sub(\"abcABC\",1,2)" )

    ; T9 := Text . Sub ( T5 , 1 , 3 ) 
    ; Check ( T9 , "Sub(\"abcABC\",1,3)" )

    ; T10 := Text . Sub ( T5 , 1 , 4 ) 
    ; Check ( T10 , "Sub(\"abcABC\",1,4)" )

    ; T11 := Text . Sub ( T5 , 1 , 5 ) 
    ; Check ( T11 , "Sub(\"abcABC\",1,5)" )

    ; T12 := Text . Sub ( T5 , 1 , 6 ) 
    ; Check ( T12 , "Sub(\"abcABC\",1,6)" )

    ; T13 := Text . Sub ( T5 , 3 , 1 ) 
    ; Check ( T13 , "Sub(\"abcABC\",3,1)" )

    ; T14 := Text . Sub ( T5 , 3 , 2 ) 
    ; Check ( T14 , "Sub(\"abcABC\",3,2)" )

    ; T15 := Text . Sub ( T5 , 3 , 4 ) 
    ; Check ( T15 , "Sub(\"abcABC\",3,4)" )

    ; T15 := Text . Sub ( T5 , 5 , 4 ) 
    ; Check ( T15 , "Sub(\"abcABC\",5,4)" )

    ; T15 := Text . Sub ( T5 , 5 , 1 ) 
    ; Check ( T15 , "Sub(\"abcABC\",5,1)" )

    ; T15 := Text . Sub ( T5 , 4 , 1 ) 
    ; Check ( T15 , "Sub(\"abcABC\",4,1)" )

    ; T15 := Text . Sub ( T5 , 4 , 4 ) 
    ; Check ( T15 , "Sub(\"abcABC\",4,4)" )

    ; T16 := Text . Sub ( "0123456789" , 1 , 8 ) 
    ; Check ( T16 , "Sub(\"0123456789\",1,8)" )

    ; T17 := Text . Sub ( Text . Sub ( "0123456789" , 1 , 8 ) , 3 , 5 ) 
    ; Check ( T17 , "Sub(Sub(\"0123456789\",1,8),3,5)" )

    ; T18 := Text . Sub ( Text . Sub ( "0123456789" , 1 , 8 ) , 3 , 12 ) 
    ; Check ( T18 , "Sub(Sub(\"0123456789\",1,8),3,12)" )

    END Work

; PROCEDURE Summary ( ) 

  = BEGIN 
      Wr . PutText ( WrT , Fmt . Int ( GTestCt ) ) 
    ; Wr . PutText ( WrT , " tests completed." ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Fmt . Int ( GFailureCt ) ) 
    ; Wr . PutText ( WrT , " tests failed." ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; IF GFailureCt = 0 
      THEN
        Wr . PutText ( WrT , "All tests PASSED." ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ELSE
        Wr . PutText ( WrT , "Some tests FAILED." ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (* IF *) 
    ; Wr . Flush ( WrT ) 

    END Summary

; VAR ExitCode : INTEGER 

; BEGIN
    Wr . PutText 
      ( WrT , "============ Using old Text algorithms =====================" ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . Flush ( WrT ) 
  ; TextClass . Old := TRUE 
  ; Work ( )  
  ; TextClass . Old := FALSE 

  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . PutText 
      ( WrT , "============ New Text algorithms, no flattening ============" ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . Flush ( WrT ) 
  ; TextClass . Flatten := FALSE 
  ; Work ( ) 
  ; TextClass . Flatten := TRUE 

  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . PutText 
      ( WrT , "============ New Text algorithms, with flattening ==========" ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . PutText ( WrT , Wr . EOL ) 
  ; Wr . Flush ( WrT ) 
  ; Work ( ) 

  ; Summary ( ) 
  
  ; IF GFailureCt = 0 
    THEN ExitCode := 0 
    ELSE ExitCode := -1
    END (* IF *) 
  ; RTProcess . Exit ( ExitCode ) 
  END TextForAllDo
.


