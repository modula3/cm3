(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3String.i3                                           *)
(* Last modified on Mon Jun 27 10:33:54 PDT 1994 by kalsow     *)
(*      modified on Sat Feb 24 01:03:39 1990 by muller         *)

INTERFACE M3String;

IMPORT M3Buf;

TYPE T <: REFANY;

PROCEDURE Add (x: TEXT): T;
(* Returns a string equal to 'x' *)

PROCEDURE FromStr (READONLY buf: ARRAY OF CHAR;  length: INTEGER := 99999): T;
(* Returns a string equal to buf[0 .. MIN(HIGH(buf), length-1)]. *)

PROCEDURE Concat (a, b: T): T;
(* Returns the concatenation of two strings *)

PROCEDURE ToText (t: T): TEXT;
(* Returns a text equal to t *)

PROCEDURE Put (wr: M3Buf.T;  t: T);
(* writes the string on the writer *)

PROCEDURE Init_chars (offset: INTEGER;  t: T;  is_const: BOOLEAN);
(* initializes the current variable at 'offset' in the global
   data or constant pool with the characters of 't'. *)

PROCEDURE Length (t: T): INTEGER;
(* returns the length of the string *)

(* NOTE! These UIDs have nothing to do with the UIDs that are hashes
         and used many places. *)

CONST NO_UID = -1;

PROCEDURE GetUID (t: T): INTEGER;
(* returns the string's UID. NO_UID if SetUID never called. *)

PROCEDURE SetUID (t: T;  uid: INTEGER);
(* sets the string's UID *)

PROCEDURE Hash (t: T): INTEGER;
(* returns a hash value for the string *)

PROCEDURE Initialize ();
PROCEDURE Reset ();

END M3String.
