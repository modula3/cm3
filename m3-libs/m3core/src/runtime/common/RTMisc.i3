(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Tue Aug  9 13:03:20 PDT 1994 by kalsow     *)
(*|      modified on Sun Feb 21 14:12:31 PST 1993 by jdd        *)
(*|      modified on Wed Mar 13 01:21:53 1991 by muller         *)

(* "RTMisc" is a private interface. *)

INTERFACE RTMisc;

(* This interface provides access to miscellaneous runtime routines. *)

(*------------------------------- byte copying ------------------------------*)

PROCEDURE Copy (src, dest: ADDRESS; len: INTEGER);
(* copy len bytes from src to dest *)

PROCEDURE Zero (dest: ADDRESS; len: INTEGER);
(* zero len bytes begining at dest *)

(*------------------------------- rounded arithmetic ------------------------*)

PROCEDURE Align (a: ADDRESS; y: INTEGER): ADDRESS;
(* return the smallest integer greater or equal to x that is a multiple of
   y *)

PROCEDURE Upper (x, y: INTEGER): INTEGER;
(* return the smallest integer greater or equal to x that is a multiple of
   y *)

(*------------------------------- runtime error reporting -------------------*)

PROCEDURE FatalError (file: TEXT; line: INTEGER; a, b, c: TEXT := NIL);
(* report an error *)

PROCEDURE FatalErrorS (file: ADDRESS; line: INTEGER; a, b, c: TEXT := NIL);
(* report an error (file = C-style null-terminated string)  *)

PROCEDURE FatalErrorI (msg: TEXT := NIL; i: INTEGER);
(* report an error with an integer argument *)

PROCEDURE FatalErrorPC (pc: INTEGER; a, b, c: TEXT := NIL);
(* report an error at the given PC and crash *)

PROCEDURE ReportErrorPC (pc: INTEGER; a, b, c: TEXT := NIL);
(* report an error at the given PC, but don't crash *)

END RTMisc.

