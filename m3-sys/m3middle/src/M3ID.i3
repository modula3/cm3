(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3ID.i3                                               *)
(* Last modified on Fri Aug 26 15:23:05 PDT 1994 by kalsow     *)
(*      modified on Sat Feb 24 01:03:39 1990 by muller         *)

INTERFACE M3ID;

(* An "M3ID.T" represents a Modula-3 identifier.  Equal identifiers
   are represented by the same M3ID.T.  Any non-NULL character
   may be included in an "M3ID.T".  *)

IMPORT M3Buf;

TYPE  T    = [-16_7fffffff-1 .. 16_7fffffff]; (* a 32-bit integer *)
CONST NoID = 0;  (* an illegal ID, never returned by Add or FromStr *)

PROCEDURE Add (x: TEXT;  class: [0..255] := 0): T;
(* Returns the ID representing "x" and sets the class of the ID to "class". *)

PROCEDURE FromStr (READONLY buf: ARRAY OF CHAR;  length: INTEGER := 99999): T;
(* Returns the ID representing buf[0 .. MIN(HIGH(buf), length-1)]. *)

PROCEDURE ToText (t: T): TEXT;
(* Returns a text equal to the one "t" represents. *)

PROCEDURE GetClass (t: T): INTEGER;
(* Returns the class assigned to "t". *)

PROCEDURE Hash (t: T): INTEGER;
(* Returns the hash value of "t". *)

PROCEDURE Put (wr: M3Buf.T;  t: T);
(* == M3Buf.PutText (ToText(t)), without the impiled TEXT allocation *)

PROCEDURE AdvanceMarks ();
(* start using a "new" set of marks.  They're recycled after 256 advances. *)

PROCEDURE SetMark (t: T): BOOLEAN;
(* Return "TRUE" if "SetMark(t)" hash been called since the last call
   to "AdvanceMarks".  There's a chance for false positives, so clients
   must be prepared to check that "t" was indeed marked twice. *)

PROCEDURE IsLT (a, b: T): BOOLEAN;
(* Return "TRUE" if the text represented by "a" is lexicographically
   less than the one represented by "b". *)

END M3ID.
