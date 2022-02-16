(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test e041                              *)
(* Copyright 2022, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test compile-time error detection of mutually recursive type declarations,
   located in different compilations and involving revealed opaque types.
   As of 2022-2-16, it appears this will require the help of M3Linker, and
   that there has never been any attempt to handle such cases.  The front end
   is segfaulting, via runaway recursion. 
*)

MODULE Main

; IMPORT One
; IMPORT Two 
(*
; TYPE T = T OBJECT x : INTEGER END

; VAR V := T ( 0 ) 
*)

; BEGIN
    One . P1 ( )
  ; Two . P2 ( ) 
  END Main
.

