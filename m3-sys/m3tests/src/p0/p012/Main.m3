(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p012                              *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* A test case for up-level addressing, using some glass-box knowlege of
   the llvm backend's convoluted display system.
*) 

MODULE Main

; FROM Support IMPORT WT , WI , WEOL 

; VAR VisitCt0 : INTEGER := 0 
; VAR VisitCt0_0 : INTEGER := 0 
; VAR VisitCt0_0_0 : INTEGER := 0

(* So we can call indirectly, through a closure. *) 
; TYPE T0_0
  = PROCEDURE ( P0_0_F : INTEGER ; VAR P0_0_G : INTEGER ; Inc : INTEGER )

; PROCEDURE Call0_0
    ( P0_0_F : INTEGER ; VAR P0_0_G : INTEGER ; Inc : INTEGER ; Proc : T0_0 )

  = BEGIN
      Proc ( P0_0_F , P0_0_G , Inc )
    END Call0_0 

; PROCEDURE P0 ( Inc : INTEGER ) 

  = VAR P0_x : INTEGER := 3   
  ; VAR P0_y : INTEGER := 4

  ; VAR LVisitNo : INTEGER := VisitCt0

  ; PROCEDURE P0_0 ( P0_0_F : INTEGER ; VAR P0_0_G : INTEGER ; Inc : INTEGER )

    = VAR LVisitNo : INTEGER := VisitCt0_0
    ; VAR P0_0_A : INTEGER := 47

    ; PROCEDURE Dump0_0 ( Tag : TEXT ) 

      = BEGIN 
          WT ( "  P0_0, visit no. " )
        ; WI ( LVisitNo )
        ; WT ( Tag )
        ; WEOL ( )

        ; WT ( "    Inc = " )
        ; WI ( Inc )
        ; WEOL ( )

        ; WT ( "    P0_0_F = " )
        ; WI ( P0_0_F )
        ; WEOL ( )

        ; WT ( "    P0_0_G = " )
        ; WI ( P0_0_G )
        ; WEOL ( )

        ; WT ( "    P0_0_A = " )
        ; WI ( P0_0_A )
        ; WEOL ( )

        END Dump0_0 

    ; PROCEDURE P0_0_0 ( VAR P0_0_0_P , P0_0_0_Q : INTEGER ; Inc : INTEGER )

      = VAR LVisitNo : INTEGER := VisitCt0_0_0

      ; BEGIN
          WT ( "    P0_0_0, visit no. " )
        ; WI ( LVisitNo )
        ; WT ( ", at entry:" ) 
        ; WEOL ( )

        ; WT ( "      Inc = " )
        ; WI ( Inc )
        ; WEOL ( )

        ; INC ( P0_0_0_P , Inc ) 
        ; INC ( P0_0_0_Q , Inc ) 
        ; INC ( P0_0_A , Inc ) 
        ; INC ( P0_x , Inc ) 
        ; INC ( P0_w , Inc ) 

        ; INC ( VisitCt0_0_0 ) 
        END P0_0_0 

    ; BEGIN (* P0_0 *)
        Dump0_0 ( ", at entry:" )
      ; INC ( P0_0_F , Inc ) 
      ; INC ( P0_0_G , Inc ) 
      ; INC ( P0_0_A , Inc ) 
      ; INC ( P0_x , Inc )
      ; INC ( P0_y , Inc )
      ; Dump0 ( ", inside P0_0, before call on P0_0_0:" )
      ; Dump0_0 ( ", before call on P0_0_0:" )
      ; P0_0_0 ( P0_z , P0_0_A , Inc DIV 2 ) 
      ; Dump0 ( ", inside P0_0, after call on P0_0_0:" )
      ; Dump0_0 ( ", at exit:" )
      ; INC ( VisitCt0_0 ) 
      END P0_0

  ; PROCEDURE Dump0 ( Tag : TEXT ) 

    = BEGIN
        WT ( "P0, visit no. " )
      ; WI ( LVisitNo )
      ; WT ( Tag )
      ; WEOL ( )

      ; WT ( "  Inc = " )
      ; WI ( Inc )
      ; WEOL ( )

      ; WT ( "  P0_x = " )
      ; WI ( P0_x )
      ; WEOL ( )

      ; WT ( "  P0_y = " )
      ; WI ( P0_y )
      ; WEOL ( )

      ; WT ( "  P0_z = " )
      ; WI ( P0_z )
      ; WEOL ( )

      ; WT ( "  P0_w = " )
      ; WI ( P0_w )
      ; WEOL ( )
      END Dump0

  ; BEGIN (* P0 *)
      Dump0 ( ", at entry:" )
    ; INC ( P0_x , Inc )
    ; INC ( P0_y , Inc )
    ; Dump0 ( ", before 1st call on P0_0:" )
    ; P0_0 ( P0_z , P0_w , Inc DIV 3 ) 
    ; INC ( P0_z , Inc + 7 )
    ; INC ( P0_w , Inc + 7 )
    ; Dump0 ( ", before 2nd, indirect, call on P0_0:" )
    ; Call0_0 ( P0_x , P0_y , ( Inc * 2 ) DIV 3 , P0_0 ) 
    ; Dump0 ( ", at exit:" )
    ; INC ( VisitCt0 ) 
    END P0

  ; VAR P0_z : INTEGER := 5
  ; VAR P0_w : INTEGER := 6

; BEGIN
    P0 ( 48 ) 
  END Main
.

