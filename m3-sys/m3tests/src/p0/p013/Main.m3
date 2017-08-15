(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p013                              *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test indirect calls on procedure variables and parameters. *)

(* Includes: Calls on procedure variables (which are statically known to
               be top-level procedures.
             Calls on procedure parameters (which can be either top-level
               or nested, using direct or closure calls.
             Calls in internally-generated FINALLY blocks, which are always
               coded as nested procedures.)
             Up-level references from indirectly-called nested procedures.
*)


MODULE Main

; FROM Support IMPORT WI , WT , WEOL

; TYPE ProcTyp = PROCEDURE ( F : INTEGER ) : INTEGER 

; VAR GProc : ProcTyp 

; PROCEDURE GPlus5 ( F5 : INTEGER ) : INTEGER
  = VAR Res : INTEGER
  ; BEGIN
      Res := F5 + 5
    ; WT ( "GPlus5.Res = " ) ; WI ( Res ) ; WEOL ( ) 
    ; RETURN Res 
    END GPlus5

; PROCEDURE GPlus10 ( F10 : INTEGER ) : INTEGER
  = VAR Res : INTEGER
  ; BEGIN
      Res := F10 + 10
    ; WT ( "GPlus10.Res = " ) ; WI ( Res ) ; WEOL ( ) 
    ; RETURN Res 
    END GPlus10

; PROCEDURE P ( )

  = VAR UpLev1 , UpLev2 : INTEGER

  ; PROCEDURE NPlus15 ( F15 : INTEGER ) : INTEGER
    = VAR Res : INTEGER
    ; BEGIN
        Res := UpLev1 + UpLev2 + F15 + 15
      ; WT ( "NPlus15.Res = " ) ; WI ( Res ) ; WEOL ( ) 
      ; RETURN Res 
      END NPlus15

  ; PROCEDURE NPlus20 ( F20 : INTEGER ) : INTEGER
    = VAR Res : INTEGER
    ; BEGIN
        Res := UpLev1 - UpLev2 + F20 + 20
      ; WT ( "NPlus20.Res = " ) ; WI ( Res ) ; WEOL ( ) 
      ; RETURN Res 
      END NPlus20

  ; PROCEDURE Inner ( Param : INTEGER ; Proc : ProcTyp ) : INTEGER
    = VAR Res : INTEGER
    ; BEGIN
        Res := Proc ( Param )
      ; WT ( "Inner.Res = " ) ; WI ( Res ) ; WEOL ( ) 
      ; RETURN Res 
      END Inner

  ; VAR L1 , L2 , L3 , L4 , L5 , L6 , L7 , L8 : INTEGER 

  ; BEGIN (* P *)
      GProc := GPlus5
    ; L1 := GProc ( 0 )
    ; GProc := GPlus10 
    ; L2 := GProc ( 1 )
    ; UpLev1 := 30
    ; UpLev2 := 4 
    ; L3 := Inner ( 3 , NPlus15 ) 
    ; L4 := Inner ( 4 , NPlus20 )
    
    ; UpLev1 := 40
    ; UpLev2 := 5
    ; L5 := Inner ( 6 , NPlus15 ) 
    ; L6 := Inner ( 7 , NPlus20 )

    ; UpLev1 := 50
    ; UpLev2 := 3
    ; L7 := Inner ( 8 , GPlus5 ) 
    ; L8 := Inner ( 9 , GPlus10 )

    ; WT ( "L1 = " ) ; WI ( L1 ) ; WEOL ( ) 
    ; WT ( "L2 = " ) ; WI ( L2 ) ; WEOL ( ) 
    ; WT ( "L3 = " ) ; WI ( L3 ) ; WEOL ( ) 
    ; WT ( "L4 = " ) ; WI ( L4 ) ; WEOL ( ) 
    ; WT ( "L5 = " ) ; WI ( L5 ) ; WEOL ( ) 
    ; WT ( "L6 = " ) ; WI ( L6 ) ; WEOL ( ) 
    ; WT ( "L7 = " ) ; WI ( L7 ) ; WEOL ( ) 
    ; WT ( "L8 = " ) ; WI ( L8 ) ; WEOL ( ) 

    END P

; PROCEDURE Q ( )

  = VAR L : INTEGER 

  ; BEGIN
      L := 7
    ; TRY
        L := 11
      ; WT ( "L = " ) ; WI ( L ) ; WEOL ( ) 
      FINALLY
        WT ( "Finally, L = " ) ; WI ( L ) ; WEOL ( ) 
      END
    END Q

; BEGIN
    P ( )
  ; Q ( ) 
  END Main
. 
