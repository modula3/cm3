(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

(* "M3WString" manages WTEXT literals *)

INTERFACE M3WString;

IMPORT M3Buf;

TYPE
  T <: REFANY;
  Char = BITS 32 FOR [0..16_10ffff];
  Buf       = ARRAY OF Char;

PROCEDURE Add (x: TEXT): T;
(* Returns a string equal to 'x' *)

PROCEDURE FromStr (READONLY buf: Buf;  length: INTEGER := 99999): T;
(* Returns a string equal to buf[0 .. MIN(HIGH(buf), length-1)]. *)

PROCEDURE Concat (a, b: T): T;
(* Returns the concatenation of two strings *)

PROCEDURE ToLiteral (t: T): TEXT;
(* Returns a literal equal to t *)

PROCEDURE PutLiteral (wr: M3Buf.T;  t: T);
(* writes a literal representation of the string on the writer *)

PROCEDURE Init_chars (offset: INTEGER;  t: T;  is_const: BOOLEAN);
(* initializes the current variable at 'offset' in global data or
   constant pool with the characters of 't'. *)

PROCEDURE Length (t: T): INTEGER;
(* returns the length of the string *)

PROCEDURE GetUID (t: T): INTEGER;
(* returns the string's UID *)

PROCEDURE SetUID (t: T;  uid: INTEGER);
(* sets the string's UID *)

PROCEDURE Hash (t: T): INTEGER;
(* returns a hash value for the string *)

PROCEDURE Initialize ();
PROCEDURE Reset ();

END M3WString.
