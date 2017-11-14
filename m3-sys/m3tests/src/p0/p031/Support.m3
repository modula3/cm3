(* ----------------------------------------------------------------------1- *)
(* File Support.m3 for Modula3 compiler test p031                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Support

; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; PROCEDURE WL ( T : TEXT )
  = <* FATAL Thread.Alerted , Wr . Failure *>
    BEGIN
      Wr . PutText ( Stdio . stdout , T ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END WL 

; BEGIN
  END Support
. 
