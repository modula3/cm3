(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p268                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test of record constructor components that have RT assignability
   errors (i.e. assignability to their fields, statically inevitable,
   but only warned at compile time.  Only for constant record constructors,
   which were not producing runtime errors when executed.
*)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; IMPORT Lib 

; TYPE Rng = [ 0 .. 15 ]

; TYPE Rec = RECORD X , Y , Z , W : Rng END 

; CONST GC = Rec { 10 , 25 , 16 , 9 }
(* 
; VAR GVLibGC := Lib.GC (* CT or RT error *)

; VAR GVGC := GC (* CT or RT error *)

; VAR GV := Rec { 10 , 25 , 17 , 9 } (* CT or RT error *)
*)

; PROCEDURE AcceptRec ( <*UNUSED*>F : Rec )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptRec 

; PROCEDURE PassInline ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptRec ( Rec { 10 , 25 , 18 , 9 } (* RT error *) )
    END PassInline

; PROCEDURE PassLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Rec { 10 , 25 , 266 , 9 }

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptRec ( LC (* RT error *) )
    END PassLocConst

; PROCEDURE PassGlobalConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptRec ( GC (* RT error *) )
    END PassGlobalConst

; PROCEDURE PassImportConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptRec ( Lib.GC (* RT error *) )
    END PassImportConst

; PROCEDURE AssignInline ( ) (*RAISES ANY*) 
  = VAR LV : Rec 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LV := Rec { 10 , 25 , 18 , 9 } (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignInline

; PROCEDURE AssignLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Rec { 10 , 25 , 266 , 9 }

  ; VAR LVLC : Rec 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLC := LC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignLocConst

; PROCEDURE AssignGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC : Rec

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVGC := GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignGlobalConst

; PROCEDURE AssignImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC : Rec 
  
  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLibGC := Lib.GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignImportConst

; PROCEDURE VarInline ( ) (*RAISES ANY*) 
  = VAR LV := Rec { 10 , 25 , 18 , 9 } (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LV . Y  
    ; I := 7 (* Just for a breakpoint. *)
    END VarInline

; PROCEDURE VarLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Rec{ 10 , 25 , 266 , 9 }

  ; VAR LVLC := LC (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVLC . Y 
    ; I := 7 (* Just for a breakpoint. *)
    END VarLocConst

; PROCEDURE VarGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC := GC (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVGC . Y 
    ; I := 7 (* Just for a breakpoint. *)
    END VarGlobalConst

; PROCEDURE VarImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC := Lib.GC (* RT error *)
  
  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVLibGC . Y 
    ; I := 7 (* Just for a breakpoint. *)
    END VarImportConst

; <*UNUSED*> PROCEDURE NoRTError ( ) (*RAISES ANY*) 
  = BEGIN
    END NoRTError 

; TYPE ProcTyp = PROCEDURE ( ) RAISES ANY 

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
  ; TestMustFail ( PassInline , "pass inline constant" ) 
  ; TestMustFail ( PassLocConst , "pass local constant" ) 
  ; TestMustFail ( PassGlobalConst , "pass global constant" ) 
  ; TestMustFail ( PassImportConst , "pass imported constant" ) 

  ; TestMustFail ( VarInline , "variable, initialized inline" )
  ; TestMustFail ( VarLocConst , "variable, initialized to local constant" )
  ; TestMustFail ( VarGlobalConst , "variable, initialized to global constant" )
  ; TestMustFail ( VarImportConst , "variable, initialized to imported constant" )

  ; TestMustFail ( AssignInline , "variable, assigned inline" )
  ; TestMustFail ( AssignLocConst , "variable, assigned to local constant" )
  ; TestMustFail ( AssignGlobalConst , "variable, assigned to global constant" )
  ; TestMustFail ( AssignImportConst , "variable, assigned to imported constant" )

(* This is just to test reporting of failures: 
  ; TestMustFail ( NoRTError , "Actually, should fail to fail." )
*)  
  ; Report ( ) 
  END Main
.
