(* ----------------------------------------------------------------------1- *)
(* File RefInst.m3 for Modula3 compiler test p031                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE RefInst

; TYPE RT = REF INTEGER

; PROCEDURE Init ( )
  = VAR LV1 , LV2 : RT
  ; BEGIN
      LV1 := NEW ( RT )
    ; LV1 ^ := 18 
    ; Val1 := LV1  

    ; LV2 := NEW ( RT )
    ; LV2 ^ := 5 
    ; Val2 := LV2  
    END Init

; BEGIN
    Init ( ) 
  END RefInst
. 
