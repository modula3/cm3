(* ----------------------------------------------------------------------1- *)
(* File Second.m3 for Modula3 compiler test p287                            *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Second

; IMPORT Wr 

; IMPORT Def
; IMPORT Display 

; PROCEDURE InterfaceCalls ( )
  = BEGIN
      Display . Put ( "Calls from Second.m3" ) 
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
  END Second
.



