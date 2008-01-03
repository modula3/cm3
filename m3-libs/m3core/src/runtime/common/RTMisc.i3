(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)

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

END RTMisc.

