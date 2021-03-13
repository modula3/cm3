(* ----------------------------------------------------------------------1- *)
(* File Main.i3 for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test of array constructors as constants and global variable initial values *) 

MODULE Main

; IMPORT Stdio

; IMPORT Support 
; IMPORT UnsafeUtils 

; IMPORT Constants
; IMPORT Dynamic
; IMPORT Globals 

; VAR ProgLabelMainVar := "p269: Array constructors"

; PROCEDURE Work2 ( )
  = BEGIN
      Constants . Work ( ) 
    ; Dynamic . Work ( ) 
    ; Support . Report ( ) 
    END Work2 

; PROCEDURE Work1 ( )
  = <*UNUSED*> VAR LotsaSpace : ARRAY [ 0 .. 255 ] OF INTEGER
        (* So dope constructed during passing of array actuals will be
           deeper on the stack than LocVar. *)
  ; BEGIN
      Work2 ( ) 
    END Work1 

; PROCEDURE Work ( )
  = VAR LocVar : INTEGER
    (* Near bottom of stack. *) 
  ; BEGIN
      UnsafeUtils . Init ( LocVar ) 
    ; Support . WrT := Stdio . stdout
    ; Support . Reset ( )
    ; Support . StartProg ( ProgLabelMainVar ) 
    ; Globals . GDisplaySuccess := FALSE     
    ; Work1 ( )     
    END Work 

; BEGIN
    Work ( ) 
  END Main
. 
