(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Nov 18 15:37:18 PST 1994 by kalsow  *)
(*      modified on Sat Oct  6 02:01:36 1990 by muller      *)

(* RTIO is a simple low-level interface for I/O used by the
   runtime.  None of its routines do any locking or require
   any memory allocation.  Output is buffered internally
   until "Flush" is called or the internal buffer overflows.

   Clients beware, this interface may change at any time.
*)

INTERFACE RTIO;

PROCEDURE PutChar (c: CHAR);
(* Write "c". *)

PROCEDURE PutChars (a: ADDRESS;  n: INTEGER);
(* Write characters "a[0..n-1]". *)

PROCEDURE PutString (s: ADDRESS);
(* Write the null terminated string beginning at "s". *)

PROCEDURE PutText (t: TEXT);
(* Write text "t" on file "f". *)

PROCEDURE PutInt (i: INTEGER;  width := 0);
(* Convert integer "i" to decimal digits and write it right-justified
   in a field of "width" characters. *)

PROCEDURE PutHex (i: INTEGER;  width := 0);
(* Convert unsigned integer "i" to hexidecimal digits with a "0x" prefix
   and write it right-justified in a field of "width" characters. *)

PROCEDURE PutAddr (a: ADDRESS;  width := 0);
(* == PutHex (LOOPHOLE (a, INTEGER), width) *)

PROCEDURE Flush ();
(* Flush any buffered characters to the operating system. *)

END RTIO.
