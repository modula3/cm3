(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p288                              *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test constant array, record, and set constructors,
   occurring in default values of record fields and
   referenced from various compilation units. *)

MODULE Main

; IMPORT First
; IMPORT Second 

; BEGIN
    First . Work ( ) 
  ; Second . Work ( ) 
  END Main
.
