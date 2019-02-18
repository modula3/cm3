(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 09:46:59 PST 1994 by kalsow     *)

INTERFACE Wx;

(* An "Wx.T" is an in-memory, append-only buffer.  It's cheaper
   to write strings and numbers to an "Wx.T" than a "Wr.T".

   It's an unchecked runtime error to concurrently call procedures
   in this interface on a single "Wx.T".
*)

TYPE T <: REFANY;

PROCEDURE New (): T;

PROCEDURE PutChar (t: T;  ch: CHAR);
PROCEDURE PutText (t: T;  txt: TEXT);
PROCEDURE PutInt  (t: T;  i: INTEGER);
PROCEDURE PutStr  (t: T;  READONLY x: ARRAY OF CHAR);

PROCEDURE ToText   (t: T): TEXT;

END Wx.
