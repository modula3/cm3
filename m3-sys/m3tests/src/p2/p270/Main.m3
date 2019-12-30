(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p270                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test of array constructor components that have RT assignability
   errors (i.e. assignability to their elementss, statically inevitable,
   but only warned at compile time.  Only for constant array constructors,
   which were not producing runtime errors when executed.
*)

(* This produces a bunch of compile-time warnings, but the real test is
   for runtime behavior.
*)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; IMPORT Lib 

; TYPE Rng = [ 0 .. 15 ]

; TYPE Arr = ARRAY [ 0 .. 3 ] OF Rng 

; CONST GC = Arr { 10 , 25 , 16 , 9 }
(* 
; VAR GVLibGC := Lib.GC (* CT or RT error *)

; VAR GVGC := GC (* CT or RT error *)

; VAR GV := Arr { 10 , 25 , 17 , 9 } (* CT or RT error *)
*)

; PROCEDURE AcceptArr ( <*UNUSED*>F : Arr )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptArr 

; PROCEDURE AcceptArrDefault ( <*UNUSED*> F := Arr { 10 , 25 , 18 , 9 } )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptArrDefault 

; PROCEDURE AcceptArrRO ( <*UNUSED*> READONLY F : Arr )
  = VAR I : INTEGER
  ; BEGIN
      I := 11 (* Just for a breakpoint. *)
    END AcceptArrRO 

; PROCEDURE PassInline ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArr ( Arr { 10 , 25 , 18 , 9 } (* RT error *) )
    END PassInline

; PROCEDURE PassLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Arr { 10 , 25 , 266 , 9 }

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArr ( LC (* RT error *) )
    END PassLocConst

; PROCEDURE PassGlobalConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArr ( GC (* RT error *) )
    END PassGlobalConst

; PROCEDURE PassImportConst ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArr ( Lib.GC (* RT error *) )
    END PassImportConst

; PROCEDURE PassDefault ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArrDefault ( (* RT error *) )
    END PassDefault 

; PROCEDURE PassRO ( ) (*RAISES ANY*) 
  = VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; AcceptArrRO ( Arr { 10 , 25 , 18 , 9 } (* RT error *) )
    END PassRO

; PROCEDURE AssignInline ( ) (*RAISES ANY*) 
  = VAR LV : Arr 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LV := Arr { 10 , 25 , 18 , 9 } (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignInline

; PROCEDURE AssignLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Arr { 10 , 25 , 266 , 9 }

  ; VAR LVLC : Arr 

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLC := LC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignLocConst

; PROCEDURE AssignGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC : Arr

  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVGC := GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignGlobalConst

; PROCEDURE AssignImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC : Arr 
  
  ; VAR I : INTEGER 
  ; BEGIN
      I := 7
    ; LVLibGC := Lib.GC (* RT error *)
    ; I := 9 (* Just for a breakpoint. *)
    END AssignImportConst

; PROCEDURE VarInline ( ) (*RAISES ANY*) 
  = VAR LV := Arr { 10 , 25 , 18 , 9 } (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LV [ 0 ] 
    ; I := 7 (* Just for a breakpoint. *)
    END VarInline

; PROCEDURE VarLocConst ( ) (*RAISES ANY*) 
  = CONST LC = Arr { 10 , 25 , 266 , 9 }

  ; VAR LVLC := LC (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVLC [ 0 ] 
    ; I := 7 (* Just for a breakpoint. *)
    END VarLocConst

; PROCEDURE VarGlobalConst ( ) (*RAISES ANY*) 
  = VAR LVGC := GC (* RT error *)

  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVGC [ 0 ] 
    ; I := 7 (* Just for a breakpoint. *)
    END VarGlobalConst

; PROCEDURE VarImportConst  ( ) (*RAISES ANY*) 
  = VAR LVLibGC := Lib.GC (* RT error *)
  
  ; VAR I , J : INTEGER 
  ; BEGIN
      J := LVLibGC [ 0 ] 
    ; I := 7 (* Just for a breakpoint. *)
    END VarImportConst

; PROCEDURE SubscriptInline ( ) (*RAISES ANY*) 
  = VAR I , J : INTEGER 
  ; BEGIN
      J := Arr { 10 , 25 , 18 , 9 } (* RT error *) [ 0 ] 
    ; I := 7 (* Just for a breakpoint. *)
    END SubscriptInline

; TYPE ProcTyp = PROCEDURE ( ) RAISES ANY  
; TYPE ArrOfProcTyp = ARRAY [ 0 .. 0 ] OF ProcTyp  
; TYPE OpenArrOfProcTyp = ARRAY  OF ProcTyp  

; PROCEDURE ProcFixed  ( ) RAISES ANY  
  = PROCEDURE ThisProcIsNested ( )
    = VAR K : INTEGER
    ; BEGIN
        K := 11 (* Just for a breakpoint. *)
      END ThisProcIsNested 
  
  ; VAR I : INTEGER := 5  
  ; VAR Arr := ArrOfProcTyp { ThisProcIsNested }
  ; BEGIN
      I := 7 (* Just for a breakpoint. *)
    ; Arr [ 0 ] ( ) 
    ; I := 9 (* Just for a breakpoint. *)
    END ProcFixed 

; PROCEDURE ProcOpenSs  ( ) RAISES ANY 
  = PROCEDURE ThisProcIsNested ( )
    = VAR K : INTEGER
    ; BEGIN
        K := 11 (* Just for a breakpoint. *)
      END ThisProcIsNested 
  
  ; VAR I : INTEGER := 5  
  ; VAR ArrRef : REF OpenArrOfProcTyp := NEW ( REF OpenArrOfProcTyp , 1 )
  ; BEGIN
      I := 7 (* Just for a breakpoint. *)
    ; ArrRef ^ [ 0 ] := ThisProcIsNested  
    ; ArrRef ^ [ 0 ] ( ) 
    ; I := 9 (* Just for a breakpoint. *)
    END ProcOpenSs 

; PROCEDURE ProcOpenConstr ( ) RAISES ANY  
  = PROCEDURE ThisProcIsNested ( )
    = VAR K : INTEGER
    ; BEGIN
        K := 11 (* Just for a breakpoint. *)
      END ThisProcIsNested 
  
  ; VAR I : INTEGER := 5  
  ; VAR ArrRef : REF OpenArrOfProcTyp := NEW ( REF OpenArrOfProcTyp , 1 )
  ; BEGIN
      I := 7 (* Just for a breakpoint. *)
    ; ArrRef ^ := OpenArrOfProcTyp { ThisProcIsNested }  
    ; ArrRef ^ [ 0 ] ( ) 
    ; I := 9 (* Just for a breakpoint. *)
    END ProcOpenConstr 

; PROCEDURE WithInline ( ) 
  = VAR I , J : INTEGER
  ; BEGIN
      WITH W = Arr { 10 , 25 , 18 , 9 } (* RT error *) 
      DO
        J := W [ 0 ] 
      ; I := 9 (* Just for a breakpoint. *)
      END 
    END WithInline 

; PROCEDURE WithGlobal ( ) 
  = VAR I , J : INTEGER
  ; BEGIN
      WITH W = GC
      DO
        J := W [ 0 ] 
      ; I := 9 (* Just for a breakpoint. *)
      END 
    END WithGlobal 

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

  ; TestMustFail ( SubscriptInline , "directly subscripted constructor" )

  ; TestMustFail
      ( ProcFixed
      , "nested procedure assigned element of fixed array variable"
      ) 
  ; TestMustFail
      ( ProcOpenSs
      , "nested procedure assigned to element of open array heap object"
      )  
  ; TestMustFail
      ( ProcOpenConstr
      , "nested procedure ss element of open array constrctor"
      )  

  ; TestMustFail ( WithInline , "WITH-bound global constant" )
  ; TestMustFail ( WithGlobal , "WITH-bound inline constructor" )

(* This is just to test reporting of failures: 
  ; TestMustFail ( NoRTError , "Actually, should fail to fail." )
*)  
  ; Report ( ) 
  END Main
.
