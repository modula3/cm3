(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Buf.i3                                              *)
(* Last modified on Wed Feb 22 08:39:49 PST 1995 by kalsow     *)
(*      modified on Tue May 25 14:25:23 PDT 1993 by muller     *)

INTERFACE M3Buf;

IMPORT Wr, Target, M3FP;

(* An "M3Buf.T" is an in-memory, append-only buffer.  It's
   cheaper to write strings and numbers to an "M3Buf.T" than
   a "Wr.T".

   It's an unchecked runtime error to concurrently call procedures
   in this interface on a single "M3Buf.T".
*)

TYPE T <: REFANY;

PROCEDURE New (): T;

PROCEDURE PutChar  (t: T;  ch: CHAR);
PROCEDURE PutText  (t: T;  txt: TEXT);
PROCEDURE PutInt   (t: T;  i: INTEGER);
PROCEDURE PutIntt  (t: T;  READONLY i: Target.Int);
PROCEDURE PutFloat (t: T;  READONLY f: Target.Float);
PROCEDURE PutSub   (t: T;  READONLY x: ARRAY OF CHAR);

PROCEDURE ToText   (t: T): TEXT;
PROCEDURE ToFP     (t: T): M3FP.T;
PROCEDURE Flush    (t: T;  wr: Wr.T);

PROCEDURE AttachDrain (t: T;  wr: Wr.T);
(* Buffers with non-NIL drains attached will dump their contents to the
   drain rather than allocate more memory.  ToText and ToFP are not useful
   on buffers with non-NIL drains. *)

END M3Buf.
