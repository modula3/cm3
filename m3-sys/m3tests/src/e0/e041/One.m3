(* ----------------------------------------------------------------------1- *)
(* File One.m3 for Modula3 compiler test e041                               *)
(* Copyright 2022, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE One

; IMPORT Two

; REVEAL T1 = Two . T2 BRANDED "T1" OBJECT F1 : INTEGER END

; VAR V1 := NEW ( T1 , F1 := 1 ) 

; PROCEDURE P1 ( )
  = VAR I1 : INTEGER 
  ; BEGIN
      I1 := V1 . F1
    ; EVAL I1 
    END P1 

; BEGIN
  END One

.

