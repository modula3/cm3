(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p272                              *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test of set constructor components that have RT assignability
   errors (i.e. assignability to the element type, statically inevitable,
   but only warned at compile time.  Only for constant set constructors,
   which were not producing runtime errors when executed.
*)

(* This produces a bunch of compile-time warnings, but the real test is
   for runtime behavior.
*)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; IMPORT Lib

; TYPE Rng = [ 0 .. 15 ]

; TYPE Set = SET OF Rng

; TYPE BigRng = [ 0 .. 95 ] 
; TYPE BigSet = SET OF BigRng

; CONST GC = Set { 10 , 25 , 16 , 9 } (* CT Warning. *)
(* 

; VAR GVGC := GC (* CT or RT error *)

; VAR GV := Set { 10 , 25 , 17 , 9 } (* CT or RT error *)
*)

; CONST A = SET OF Rng { 1 , 2 }
; CONST B = SET OF Rng { 3 , 4 }
; CONST AB = A + B
; CONST C = SET OF Rng { 5 , 6 }
; CONST D = SET OF Rng { 7 , 8 }
; CONST CD = C + D 
; CONST ABCD = AB + CD

; PROCEDURE AcceptSet ( <*UNUSED*>F : Set )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptSet 

; PROCEDURE AcceptSetDefault
    ( <*UNUSED*>F := Set { 10 , 25 , 18 , 9 } (* CT Warning. *))
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptSetDefault

; PROCEDURE AcceptSetRO ( <*UNUSED*> READONLY F : Set )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptSetRO

; PROCEDURE PassInline ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptSet ( Set { 10 , 25 , 18 , 9 } (* CT Warning. *) (* RT error *) )
    END PassInline

; PROCEDURE PassLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Set { 10 , 25 , 266 , 9 } (* CT Warning. *)

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptSet ( LC (* RT error *) )
    END PassLocConst

; PROCEDURE PassGlobalConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptSet ( GC (* RT error *) )
    END PassGlobalConst

; PROCEDURE PassImportConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptSet ( Lib.GC (* RT error *) )
    END PassImportConst

; PROCEDURE PassDefault ( ) (*RAISES ANY*)
  = VAR I : INTEGER
  ; BEGIN
      I := 7
    ; AcceptSetDefault ( (* RT error *) )
    END PassDefault

; PROCEDURE PassRO ( ) (*RAISES ANY*)
  = VAR I : INTEGER
  ; BEGIN
      I := 7
    ; AcceptSetRO ( Set { 10 , 25 , 18 , 9 } (* CT Warning. *) (* RT error *) )
    END PassRO

; PROCEDURE AssignInline ( ) (*RAISES ANY*) 
  = VAR LV : Set 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LV := Set { 10 , 25 , 18 , 9 } (* CT Warning. *) (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignInline

; PROCEDURE AssignLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Set { 10 , 25 , 266 , 9 } (* CT Warning. *)

  ; VAR LVLC : Set 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLC := LC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignLocConst

; PROCEDURE AssignGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC : Set

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVGC := GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignGlobalConst

; PROCEDURE AssignImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC : Set 
  
  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLibGC := Lib.GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignImportConst

; PROCEDURE VarInline ( ) (*RAISES ANY*) 
  = VAR LV := Set { 10 , 25 , 18 , 9 } (* CT Warning. *) (* RT error *)

  ; VAR B : BOOLEAN 
  ; VAR I : INTEGER 
  ; BEGIN
      B := 25 IN LV 
    ; I := 7 (* Just for a breakpoint. *)
    END VarInline

; PROCEDURE VarLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Set{ 10 , 25 , 266 , 9 } (* CT Warning. *)

  ; VAR LVLC := LC (* RT error *)

  ; VAR B : BOOLEAN 
  ; VAR I : INTEGER 
  ; BEGIN
      B := 25 IN LVLC 
    ; I := 7 (* Just for a breakpoint. *)
    END VarLocConst

; PROCEDURE VarGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC := GC (* RT error *)

  ; VAR B : BOOLEAN 
  ; VAR I : INTEGER 
  ; BEGIN
      B := 25 IN LVGC 
    ; I := 7 (* Just for a breakpoint. *)
    END VarGlobalConst

; PROCEDURE VarImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC := Lib.GC (* RT error *)
  
  ; VAR B : BOOLEAN 
  ; VAR I : INTEGER 
  ; BEGIN
      B := 25 IN LVLibGC 
    ; I := 7 (* Just for a breakpoint. *)
    END VarImportConst

; EXCEPTION OkExcSmall ( Set )  
; EXCEPTION OkExcBig ( BigSet )  

; PROCEDURE RaiseSmall ( ) RAISES ANY
  = VAR I : INTEGER
  ; BEGIN
      RAISE OkExcSmall ( GC )   
    ; I := 7 (* Just for a breakpoint. *)
    END RaiseSmall 

; PROCEDURE RaiseBig ( ) RAISES ANY
  = VAR I : INTEGER
  ; VAR Last := LAST ( BigRng )
  ; VAR LBig := BigSet { 0 , LAST ( BigRng ) + 1 } 
  ; BEGIN
      RAISE OkExcBig ( LBig )   
    ; I := 7 (* Just for a breakpoint. *)
    END RaiseBig  

; TYPE ProcTyp = PROCEDURE ( ) RAISES ANY

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

; PROCEDURE TestMustFail ( Proc : ProcTyp ; Label : TEXT )

  = BEGIN
      TRY
        Proc ( )
      ; WL ( "##### FAILED ##### to raise an expected exception: " , Label )
      ; INC ( FailureCt )
      EXCEPT
      | OkExcSmall ( Small ) => 
        WL ( "##### FAILED ##### to raise an expected exception: " , Label )
      ; INC ( FailureCt )
      | OkExcBig ( Big ) => 
        WL ( "##### FAILED ##### to raise an expected exception: " , Label )
      ; INC ( FailureCt )
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
  ; TestMustFail ( PassDefault , "pass defaulted parameter" )
  ; TestMustFail ( PassRO , "pass readonly" )

  ; TestMustFail ( VarInline , "variable, initialized inline" )
  ; TestMustFail ( VarLocConst , "variable, initialized to local constant" )
  ; TestMustFail ( VarGlobalConst , "variable, initialized to global constant" )
  ; TestMustFail ( VarImportConst , "variable, initialized to imported constant" )

  ; TestMustFail ( AssignInline , "variable, assigned inline" )
  ; TestMustFail ( AssignLocConst , "variable, assigned to local constant" )
  ; TestMustFail ( AssignGlobalConst , "variable, assigned to global constant" )
  ; TestMustFail ( AssignImportConst , "variable, assigned to imported constant" )
  
  ; TestMustFail ( RaiseSmall , "raise exception with small set" )
  ; TestMustFail ( RaiseBig , "raise exception with big set" )

(* This is just to test reporting of failures: 
  ; TestMustFail ( NoRTError , "Actually, should fail to fail." )
*)  
  ; Report ( ) 
  END Main
.
