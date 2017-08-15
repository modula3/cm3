(* ----------------------------------------------------------------------1- *)
(* File Support.i3 for Modula3 compiler test p013                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Support is just to get distracting stuff out of main program, in case
   its IRs need to be examined.
*) 

INTERFACE Support

; IMPORT Wr

; VAR Stream : Wr . T (* Defaults to Stdio . stdout *) 

; PROCEDURE WT ( TextVal : TEXT )
  (* Write TextVal on Stream.  TextVal = NIL is OK. *) 

; PROCEDURE WI ( IntVal : INTEGER )
  (* Format and write IntVal on Stream. *) 

; PROCEDURE WEOL ( )
  (* Write end-of-line on Stream. *) 

; END Support
.




