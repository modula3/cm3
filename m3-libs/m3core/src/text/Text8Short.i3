(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* A "Text8Short.T" is a text containing at most "MaxLength"
   8-bit or "CHAR" characters. *)

INTERFACE Text8Short;

CONST MaxLength = 15;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    len     : CARDINAL;
    contents: ARRAY [0..MaxLength] OF CHAR;
  END;
  (* The array contains the characters of the text followed by a '\000'. *)

PROCEDURE New (READONLY a: ARRAY OF CHAR): T;
(* Return a new text containing the characters of "a".  It is a checked
   runtime error if "NUMBER (a) > MaxLength". *)

END Text8Short.

