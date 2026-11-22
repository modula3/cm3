(* ----------------------------------------------------------------------1- *)
(* File Two.m3 for Modula3 compiler test e041                               *)
(* Copyright 2022, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Two

; IMPORT One

; REVEAL T2 = One . T1 BRANDED "T2" OBJECT F2 : INTEGER END

; VAR V2 := NEW ( T2 , F2 := 2 )

; PROCEDURE P2 ( )
  = VAR I2 : INTEGER 
  ; BEGIN
      I2 := V2 . F2
    ; EVAL I2 
    END P2 

; BEGIN
    P2 ( ) 
  END Two

.

