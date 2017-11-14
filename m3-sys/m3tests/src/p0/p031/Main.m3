(* ----------------------------------------------------------------------1- *)
(* File Main.3 for Modula3 compiler test p031                               *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test of parameter modes, except arrays *) 

MODULE Main

; IMPORT Support 

; IMPORT TestOrd 
; IMPORT TestFloat 
; IMPORT TestRef
; IMPORT TestProc 
; IMPORT TestSets 
; IMPORT TestRecs 

; PROCEDURE Work ( )
  = VAR LFailures : BOOLEAN

  ; BEGIN
      LFailures := FALSE 
    ; TestOrd . Work ( LFailures ) 
    ; TestFloat . Work ( LFailures ) 
    ; TestRef . Work ( LFailures ) 
    ; TestProc . Work ( LFailures ) 
    ; TestSets . Work ( LFailures ) 
    ; TestRecs . Work ( LFailures )
    ; IF LFailures
      THEN
        Support . WL ( "Some cases ######FAILED######." ) 
      END (* IF *) 
    END Work 

; BEGIN
    Work ( ) 
  END Main
. 
