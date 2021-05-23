(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 09:43:39 PST 1994 by kalsow     *)

INTERFACE ID;

(* An "ID.T" represents an identifier.  Equal identifiers
   are represented by the same "ID.T".  Any non-NULL character
   may be included in an "ID.T".  *)

IMPORT Wx, Wr, Thread;

TYPE  T    = [-16_7fffffff-1 .. 16_7fffffff]; (* a 32-bit integer *)
CONST NoID: T = 0;  (* an illegal ID, never returned by Add or FromStr *)

PROCEDURE Add (x: TEXT): T;
(* Returns the ID representing "x". *)

PROCEDURE FromStr (READONLY buf: ARRAY OF CHAR;  length: INTEGER := 99999): T;
(* Returns the ID representing buf[0 .. MIN(HIGH(buf), length-1)]. *)

PROCEDURE ToText (t: T): TEXT;
(* Returns a text equal to the one "t" represents. *)

PROCEDURE Hash (t: T): INTEGER;
(* Returns the hash value of "t". *)

PROCEDURE Put (wr: Wx.T;  t: T)  RAISES {Wr.Failure, Thread.Alerted};
(* == wr.put (ToText(t)), without the impiled TEXT allocation *)

PROCEDURE IsLT (a, b: T): BOOLEAN;
(* Return "TRUE" if the text represented by "a" is lexicographically
   less than the one represented by "b". *)

PROCEDURE Compare (a, b: T): [-1 .. +1];
(* Return "-1" ("0", "+1") if the text represent by "a" is
   lexicographically less than (equal, greater than) the one
   represented by "b". *)

END ID.
