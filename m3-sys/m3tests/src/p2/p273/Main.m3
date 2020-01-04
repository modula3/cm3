(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p273                              *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test mixtures of set, record, and array constructors (when used as
   components of other constructors) that have RT assignability errors
   (i.e. assignability to their fields, statically inevitable, but
   only warned at compile time.  Only for constant component and
   containing constructors  *)

(* This produces a bunch of compile-time warnings, but the real test is
   for runtime behavior.
*)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; TYPE Rng = [ 0 .. 15 ]

; TYPE Rec = RECORD X , Y , Z , W : Rng END

; TYPE Arr = ARRAY [ 0 .. 3 ] OF Rng 

; TYPE Set = SET OF Rng 

; CONST GRec = Rec { 10 , 25 , 16 , 9 } (* CT Warning. *)
; CONST GArr = Arr { 10 , 25 , 16 , 9 } (* CT Warning. *)
; CONST GSet = Set { 10 , 25 , 16 , 9 } (* CT Warning. *)


; PROCEDURE InlineSetInRec   ( ) (*RAISES ANY*)
  = VAR Loc := RECORD FO : Set END { Set { 28 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineSetInRec

; PROCEDURE NamedSetInRec   ( ) (*RAISES ANY*)
  = VAR Loc := RECORD FO : Set END { GSet }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedSetInRec

; PROCEDURE InlineRecInRec   ( ) (*RAISES ANY*)
  = VAR Loc
      := RECORD FO : Rec END { Rec { 10 , 25 , 16 , 9 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineRecInRec

; PROCEDURE NamedRecInRec   ( ) (*RAISES ANY*)
  = VAR Loc := RECORD FO : Rec END { GRec }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedRecInRec

; PROCEDURE InlineArrInRec   ( ) (*RAISES ANY*)
  = VAR Loc
      := RECORD FO : Arr END { Arr { 10 , 25 , 16 , 9 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineArrInRec

; PROCEDURE NamedArrInRec   ( ) (*RAISES ANY*)
  = VAR Loc := RECORD FO : Arr END { GArr }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedArrInRec

; PROCEDURE InlineSetInArr   ( ) (*RAISES ANY*)
  = VAR Loc := ARRAY [ 0 .. 0 ] OF Set { Set { 28 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineSetInArr

; PROCEDURE NamedSetInArr   ( ) (*RAISES ANY*)
  = VAR Loc := ARRAY [ 0 .. 0 ] OF Set { GSet }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedSetInArr

; PROCEDURE InlineRecInArr   ( ) (*RAISES ANY*)
  = VAR Loc
      := ARRAY [ 0 .. 0 ] OF Rec { Rec { 10 , 25 , 16 , 9 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineRecInArr

; PROCEDURE NamedRecInArr   ( ) (*RAISES ANY*)
  = VAR Loc := ARRAY [ 0 .. 0 ] OF Rec { GRec }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedRecInArr

; PROCEDURE InlineArrInArr   ( ) (*RAISES ANY*)
  = VAR Loc
      := ARRAY [ 0 .. 0 ] OF Arr { Arr { 10 , 25 , 16 , 9 } (* CT Warning. *) }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END InlineArrInArr

; PROCEDURE NamedArrInArr   ( ) (*RAISES ANY*)
  = VAR Loc := ARRAY [ 0 .. 0 ] OF Arr { GArr }
  ; VAR I : INTEGER
  ; BEGIN
      EVAL Loc 
    ; I := 13 
    END NamedArrInArr

; <*UNUSED*> PROCEDURE NoRTError ( ) (*RAISES ANY*) 
  = BEGIN
    END NoRTError 

; VAR TestCt : INTEGER
; VAR FailureCt : INTEGER

; PROCEDURE Int ( I : INTEGER ) : TEXT
  = BEGIN
      RETURN Fmt . Int ( I ) 
    END Int

; VAR WrT : Wr . T
; VAR Verbose : BOOLEAN := FALSE 

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

; TYPE ProcTyp = PROCEDURE ( ) RAISES ANY

; PROCEDURE TestMustFail ( Proc : ProcTyp ; Label : TEXT )

  = BEGIN
      TRY
        Proc ( )
      ; WL ( "##### FAILED ##### to raise an expected exception: " , Label )
      ; INC ( FailureCt )
      EXCEPT
      ELSE
        IF Verbose
        THEN
          WL ( "Raised an exception, as expected: " , Label )
        END 
      END
    ; INC ( TestCt ) 
    END TestMustFail

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

; BEGIN
    TestCt := 0
  ; FailureCt := 0
  ; WrT := Stdio . stdout
  ; Verbose := TRUE
(*  Use this direct call to PassInline to see an uncaught RT error. 
  ; PassInline ( )
*)

  ; TestMustFail ( InlineSetInRec , "inline set inside record" ) 
  ; TestMustFail ( NamedSetInRec , "named set inside record" ) 

  ; TestMustFail ( InlineRecInRec , "inline record inside record" ) 
  ; TestMustFail ( NamedRecInRec , "named record inside record" ) 

  ; TestMustFail ( InlineArrInRec , "inline array inside record" ) 
  ; TestMustFail ( NamedArrInRec , "named array inside record" ) 

  ; TestMustFail ( InlineSetInArr , "inline set inside array" ) 
  ; TestMustFail ( NamedSetInArr , "named set inside array" )
  
  ; TestMustFail ( InlineRecInArr , "inline record inside array" ) 
  ; TestMustFail ( NamedRecInArr , "named record inside array" )
  
  ; TestMustFail ( InlineArrInArr , "inline array inside array" ) 
  ; TestMustFail ( NamedArrInArr , "named array inside array" ) 

(* This is just to test reporting of failures: 
  ; TestMustFail ( NoRTError , "Actually, should fail to fail." )
*)  
  ; Report ( ) 
  END Main
.
