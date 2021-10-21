(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p287                              *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test constant array, record, and set constructors,
   referenced from various compilation units. *)

MODULE Main

; IMPORT Wr 

; IMPORT Def
; IMPORT Display
; IMPORT Second 

; PROCEDURE InterfaceCalls ( )
  = BEGIN 
      Display . Put ( "Calls from Main.m3" ) 
    ; Display . Put ( Wr . EOL ) 
    ; Def . LF ( )
    
    ; Def . NF ( ) 
    ; Def . NO ( ) 
    ; Def . NR ( ) 
    ; Def . NS ( ) 

    ; Def . AF ( ) 
    ; Def . AO ( ) 
    ; Def . AR ( ) 
    ; Def . ASs ( )
        
    ; Display . Put ( Wr . EOL ) 
END InterfaceCalls 

; BEGIN
    InterfaceCalls ( )
  ; Second . InterfaceCalls ( ) 
  ; Def . InModuleCalls ( ) 
  END Main

.
