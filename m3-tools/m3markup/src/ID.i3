(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 23 07:47:28 PDT 1995 by kalsow     *)

INTERFACE ID;

(* An "ID.T" represents an identifier.  Equal identifiers
   are represented by the same "ID.T".  Any non-NULL character
   may be included in an "ID.T".  *)

IMPORT Wx;

TYPE  T    = [-16_7fffffff-1 .. 16_7fffffff]; (* a 32-bit integer *)
CONST NoID = 0;  (* an illegal ID, never returned by Add or FromStr *)

PROCEDURE Add (x: TEXT): T;
(* Returns the ID representing "x". *)

PROCEDURE FromStr (READONLY buf: ARRAY OF CHAR;  length: INTEGER := 99999): T;
(* Returns the ID representing buf[0 .. MIN(HIGH(buf), length-1)]. *)

PROCEDURE ToText (t: T): TEXT;
(* Returns a text equal to the one "t" represents. *)

PROCEDURE Hash (t: T): INTEGER;
(* Returns the hash value of "t". *)

PROCEDURE Put (wr: Wx.T;  t: T);
(* == Wx.PutText (ToText(t)), without the impiled TEXT allocation *)

PROCEDURE Compare (a, b: T): [-1 .. +1];
(* Return -1 if "a" occurs before "b", 0 if "(a = b)", +1 if
   "a" occurs after "b" in lexicographic order when case is ignored. *)

END ID.
