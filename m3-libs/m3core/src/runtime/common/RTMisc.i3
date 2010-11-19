(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)

(* "RTMisc" is a private interface. *)

INTERFACE RTMisc;

(* This interface provides access to miscellaneous runtime routines. *)

(*------------------------------- byte copying ------------------------------*)

<*EXTERNAL RTMisc__Copy*>
PROCEDURE Copy (src, dest: ADDRESS; len: INTEGER);
(* copy len bytes from src to dest *)

<*EXTERNAL RTMisc__Zero*>
PROCEDURE Zero (dest: ADDRESS; len: INTEGER);
(* zero len bytes begining at dest *)

(*------------------------------- rounded arithmetic ------------------------*)

<*EXTERNAL RTMisc__Align*>
PROCEDURE Align (a: ADDRESS; y: INTEGER): ADDRESS;
(* same as Upper but casting INTEGER <=> ADDRESS *)

PROCEDURE Upper (x, y: INTEGER): INTEGER;
(* return the smallest integer greater or equal to x that is a multiple of y *)

END RTMisc.
