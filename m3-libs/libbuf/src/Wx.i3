(* Copyright (C) 1994, Digital Equipment Corporation                   *)
(* All rights reserved.                                                *)
(* See the file COPYRIGHT for a full description.                      *)
(*                                                                     *)
(* Last modified on Thu Dec  8 09:46:59 PST 1994 by kalsow             *)
(* Last modified on Sun Feb 15 16:44:09 GMT 1998 by Thomas Frauenstein *)

INTERFACE Wx;

IMPORT Thread, Wr;

(* An "Wx.T" is an in-memory, append-only buffer.  It's cheaper
   to write strings and numbers to an "Wx.T" than a "Wr.T".

   It's an unchecked runtime error to concurrently call procedures
   in this interface on a single "Wx.T".
*)

TYPE T <: REFANY;

PROCEDURE New (): T;

PROCEDURE PutChar  (t: T;  ch: CHAR);
PROCEDURE PutText  (t: T;  a, b, c, d, e : TEXT := NIL);
PROCEDURE PutInt   (t: T;  i: INTEGER);
PROCEDURE PutStr   (t: T;  READONLY x: ARRAY OF CHAR);

PROCEDURE GetLength(t: T): INTEGER;
PROCEDURE ToText   (t: T): TEXT;
PROCEDURE ToWr     (t: T; wr : Wr.T) RAISES {Wr.Failure, Thread.Alerted};
   (* without reset *)

PROCEDURE Reset    (t: T);    
   (* reset the buffer with no explicit freeing of the allocated memory *)

END Wx.
